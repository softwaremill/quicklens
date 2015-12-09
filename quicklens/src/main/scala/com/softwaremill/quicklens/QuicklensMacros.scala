package com.softwaremill.quicklens

import scala.annotation.tailrec
import scala.reflect.macros.Context

object QuicklensMacros {
  private val ShapeInfo = "Path must have shape: _.field1.field2.each.field3.(...)"

  /**
   * modify(a)(_.b.c) => new PathMod(a, (A, F) => A.copy(b = A.b.copy(c = F(A.b.c))))
   */
  def modify_impl[T, U](c: Context)(obj: c.Expr[T])(path: c.Expr[T => U]): c.Expr[PathModify[T, U]] = {
    c.Expr[PathModify[T, U]](modify_impl_withObjTree(c)(path, obj.tree))
  }

  /**
   * a.modify(_.b.c) => new PathMod(a, (A, F) => A.copy(b = A.b.copy(c = F(A.b.c))))
   */
  def modifyPimp_impl[T, U](c: Context)(path: c.Expr[T => U]): c.Expr[PathModify[T, U]] = {
    import c.universe._

    val wrappedT = c.macroApplication match {
      case Apply(TypeApply(Select(Apply(_, List(w)), _), _), _) => w
      case _ => c.abort(c.enclosingPosition, s"Unknown usage of ModifyPimp. Please file a bug.")
    }

    val tValueName = newTermName(c.fresh())
    val tValue = q"val $tValueName = $wrappedT"

    val modification = modify_impl_withObjTree(c)(path, Ident(tValueName))

    c.Expr[PathModify[T, U]](Block(tValue, modification))
  }

  private def modify_impl_withObjTree[T, U](c: Context)(path: c.Expr[T => U], objTree: c.Tree): c.Tree = {
    import c.universe._

    sealed trait PathElement
    case class TermPathElement(term: c.TermName, xargs: c.Tree*) extends PathElement
    case class FunctorPathElement(functor: c.Tree, method: c.TermName, xargs: c.Tree*) extends PathElement

    /**
     * _.a.b.each.c => List(TPE(a), TPE(b), FPE(functor, each/at/eachWhere, xargs), TPE(c))
     */
    @tailrec
    def collectPathElements(tree: c.Tree, acc: List[PathElement]): List[PathElement] = {
      def methodSupported(method: TermName) = {
        Seq("at", "eachWhere").exists { _.equals(method.toString) }
      }
      def typeSupported(quicklensType: c.Tree) = {
        Seq("QuicklensEach", "QuicklensAt").exists { quicklensType.toString.endsWith }
      }
      tree match {
        case q"$parent.$child" => collectPathElements(parent, TermPathElement(child) :: acc)
        case q"$parent.$method(..$xargs)" if methodSupported(method) =>
          collectPathElements(parent, TermPathElement(method, xargs:_*) :: acc)
        case q"$tpname[..$x]($t)($f)" if typeSupported(tpname) =>
          val newAcc = acc match {
            // replace the term controlled by quicklens
            case TermPathElement(term, xargs @ _*) :: rest => FunctorPathElement(f, term, xargs: _*) :: rest
            case pathEl :: rest => c.abort(c.enclosingPosition, s"Invalid use of path element $pathEl. $ShapeInfo, got: ${path.tree}")
          }
          collectPathElements(t, newAcc)
        case t: Ident => acc
        case _ => c.abort(c.enclosingPosition, s"Unsupported path element. $ShapeInfo, got: $tree")
      }
    }

    /**
     * (x, List(TPE(c), TPE(b), FPE(functor, method, xargs), TPE(a))) => x.b.c
     */
    def generateSelects(rootPathEl: c.TermName, reversePathEls: List[PathElement]): c.Tree = {
      @tailrec
      def terms(els: List[PathElement], result: List[c.TermName]): List[c.TermName] = {
        els match {
          case Nil => result
          case TermPathElement(term) :: tail => terms(tail, term :: result)
          case FunctorPathElement(_, _, _*) :: _ => result
        }
      }

      @tailrec
      def go(els: List[c.TermName], result: c.Tree): c.Tree = {
        els match {
          case Nil => result
          case pathEl :: tail =>
            val select = q"$result.$pathEl"
            go(tail, select)
        }
      }

      go(terms(reversePathEls, Nil), Ident(rootPathEl))
    }

    /**
     * (a, List(TPE(d), TPE(c), FPE(functor, method, xargs), TPE(b)), k) =>
     *   (aa, aa.copy(b = functor.method(aa.b, xargs)(a => a.copy(c = a.c.copy(d = k)))
     */
    def generateCopies(rootPathEl: c.TermName, reversePathEls: List[PathElement], newVal: c.Tree): (c.TermName, c.Tree) = {
      reversePathEls match {
        case Nil => (rootPathEl, newVal)
        case TermPathElement(pathEl) :: tail =>
          val selectCurrVal = generateSelects(rootPathEl, tail)
          val selectCopy = q"$selectCurrVal.copy"
          val copy = q"$selectCopy($pathEl = $newVal)"
          generateCopies(rootPathEl, tail, copy)
        case FunctorPathElement(functor, method, xargs @ _*) :: tail =>
          val newRootPathEl = newTermName(c.fresh())
          // combine the selected path with variable args
          val args = generateSelects(newRootPathEl, tail) :: xargs.toList
          val rootPathElParamTree = ValDef(Modifiers(), rootPathEl, TypeTree(), EmptyTree)
          val functorMap = q"$functor.$method(..$args)(($rootPathElParamTree) => $newVal)"
          generateCopies(newRootPathEl, tail, functorMap)
      }
    }

    //

    val pathEls = path.tree match {
      case q"($arg) => $pathBody" => collectPathElements(pathBody, Nil)
      case _ => c.abort(c.enclosingPosition, s"$ShapeInfo, got: ${path.tree}")
    }

    // the initial root object (the end-root object can be different if there are .each's on the way)
    val initialRootPathEl = newTermName(c.fresh())
    val fn = newTermName(c.fresh()) // the function that modifies the last path element

    val reversePathEls = pathEls.reverse

    // new value of the last path element is an invocation of $fn on the current last path element value
    val select = generateSelects(initialRootPathEl, reversePathEls)
    val mod = q"$fn($select)"

    val (rootPathEl, copies) = generateCopies(initialRootPathEl, reversePathEls, mod)

    val rootPathElParamTree = ValDef(Modifiers(), rootPathEl, TypeTree(), EmptyTree)
    val fnParamTree = ValDef(Modifiers(), fn, TypeTree(), EmptyTree)

    q"com.softwaremill.quicklens.PathModify($objTree, ($rootPathElParamTree, $fnParamTree) => $copies)"
  }
}
