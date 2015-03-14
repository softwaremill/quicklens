package com.softwaremill.quicklens

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

object QuicklensMacros {
  private val ShapeInfo = "Path must have shape: _.field1.field2.each.field3.(...)"

  /**
   * modify(a)(_.b.c) => new PathMod(a, (A, F) => A.copy(b = A.b.copy(c = F(A.b.c))))
   */
  def modify_impl[T, U](c: blackbox.Context)(obj: c.Expr[T])(path: c.Expr[T => U]): c.Tree = {
    import c.universe._

    sealed trait PathElement
    case class TermPathElement(term: c.TermName) extends PathElement
    case class EachPathElement(functor: c.Tree) extends PathElement

    /**
     * _.a.b.each.c => List(TPE(a), TPE(b), EPE(functor), TPE(c))
     */
    @tailrec
    def collectPathElements(tree: c.Tree, acc: List[PathElement]): List[PathElement] = {
      tree match {
        case q"$parent.$child" => collectPathElements(parent, TermPathElement(child) :: acc)
        case q"$tpname[..$_]($t)($f)" if tpname.toString.endsWith("QuicklensEach") =>
          val accWithoutEach = acc match {
            case TermPathElement(TermName("each")) :: rest => rest
            case _ => c.abort(c.enclosingPosition, s"Invalid use of .each. $ShapeInfo, got: ${path.tree}")
          }
          collectPathElements(t, EachPathElement(f) :: accWithoutEach)
        case t: Ident => acc
        case _ =>
          c.abort(c.enclosingPosition, s"Unsupported path element. $ShapeInfo, got: $tree")
      }
    }

    /**
     * (x, List(TPE(c), TPE(b), EPE(functor), TPE(a))) => x.b.c
     */
    def generateSelects(rootPathEl: c.TermName, reversePathEls: List[PathElement]): c.Tree = {
      @tailrec
      def terms(els: List[PathElement], result: List[c.TermName]): List[c.TermName] = {
        els match {
          case Nil => result
          case TermPathElement(term) :: tail => terms(tail, term :: result)
          case EachPathElement(_) :: _ => result
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
     * (a, List(TPE(d), TPE(c), EPE(functor), TPE(b)), k) =>
     *   (aa, aa.copy(b = functor.map(aa.b)(a => a.copy(c = a.c.copy(d = k)))
     */
    def generateCopies(rootPathEl: c.TermName, reversePathEls: List[PathElement], newVal: c.Tree): (c.TermName, c.Tree) = {
      reversePathEls match {
        case Nil => (rootPathEl, newVal)
        case TermPathElement(pathEl) :: tail =>
          val selectCurrVal = generateSelects(rootPathEl, tail)
          val selectCopy = q"$selectCurrVal.copy"
          val copy = q"$selectCopy($pathEl = $newVal)"
          generateCopies(rootPathEl, tail, copy)
        case EachPathElement(functor) :: tail =>
          val newRootPathEl = TermName(c.freshName())
          val selectMapped = generateSelects(newRootPathEl, tail)
          val rootPathElParamTree = ValDef(Modifiers(), rootPathEl, TypeTree(), EmptyTree)
          val functorMap = q"$functor.map($selectMapped)(($rootPathElParamTree) => $newVal)"
          generateCopies(newRootPathEl, tail, functorMap)
      }
    }

    //

    val pathEls = path.tree match {
      case q"($arg) => $pathBody" => collectPathElements(pathBody, Nil)
      case _ => c.abort(c.enclosingPosition, s"$ShapeInfo, got: ${path.tree}")
    }

    // the initial root object (the end-root object can be different if there are .each's on the way)
    val initialRootPathEl = TermName(c.freshName())
    val fn = TermName(c.freshName()) // the function that modifies the last path element

    val reversePathEls = pathEls.reverse

    // new value of the last path element is an invocation of $fn on the current last path element value
    val select = generateSelects(initialRootPathEl, reversePathEls)
    val mod = q"$fn($select)"

    val (rootPathEl, copies) = generateCopies(initialRootPathEl, reversePathEls, mod)

    val rootPathElParamTree = ValDef(Modifiers(), rootPathEl, TypeTree(), EmptyTree)
    val fnParamTree = ValDef(Modifiers(), fn, TypeTree(), EmptyTree)
    q"com.softwaremill.quicklens.PathModify($obj, ($rootPathElParamTree, $fnParamTree) => $copies)"
  }
}
