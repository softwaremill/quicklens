package com.softwaremill.quicklens

import scala.annotation.tailrec
import scala.reflect.macros.Context

object QuicklensMacros {
  private val ShapeInfo = "Path must have shape: _.field1.field2.each.field3.(...)"

  /**
   * modify(a)(_.b.c) => new PathMod(a, (A, F) => A.copy(b = A.b.copy(c = F(A.b.c))))
   */
  def modify_impl[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(obj: c.Expr[T])(
    path: c.Expr[T => U]
  ): c.Expr[PathModify[T, U]] = modifyUnwrapped(c)(obj, modificationForPath(c)(path))

  /**
   * modifyAll(a)(_.b.c, _.d.e) => new PathMod(a, << chained modifications >>)
   */
  def modifyAll_impl[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(obj: c.Expr[T])(
    path1: c.Expr[T => U],
    paths: c.Expr[T => U]*
  ): c.Expr[PathModify[T, U]] = modifyUnwrapped(c)(obj, modificationsForPaths(c)(path1, paths))

  /**
   * A helper method for modify_impl and modifyAll_impl.
   */
  private def modifyUnwrapped[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(
    obj: c.Expr[T],
    modifications: c.Tree
  ): c.Expr[PathModify[T, U]] = {
    import c.universe._
    c.Expr[PathModify[T, U]](q"_root_.com.softwaremill.quicklens.PathModify($obj, $modifications)")
  }

  /**
   * a.modify(_.b.c) => new PathMod(a, (A, F) => A.copy(b = A.b.copy(c = F(A.b.c))))
   */
  def modifyPimp_impl[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(
    path: c.Expr[T => U]
  ): c.Expr[PathModify[T, U]] = modifyWrapped(c)(modificationForPath(c)(path))

  /**
   * a.modify(_.b.c, _.d.e) =>  new PathMod(a, << chained modifications >>)
   */
  def modifyAllPimp_impl[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(
    path1: c.Expr[T => U],
    paths: c.Expr[T => U]*
  ): c.Expr[PathModify[T, U]] = modifyWrapped(c)(modificationsForPaths(c)(path1, paths))

  /**
   * A helper method for modifyPimp_impl and modifyAllPimp_impl.
   */
  def modifyWrapped[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(
    modifications: c.Tree
  ): c.Expr[PathModify[T, U]] = {
    import c.universe._

    val wrappedValue = c.macroApplication match {
      case Apply(TypeApply(Select(Apply(_, List(w)), _), _), _) => w
      case _ => c.abort(c.enclosingPosition, s"Unknown usage of ModifyPimp. Please file a bug.")
    }

    // Here and later on: adding "0" at the end is a workaround for these issues:
    // * https://issues.scala-lang.org/browse/SI-8425
    // * https://issues.scala-lang.org/browse/SI-8533
    val valueAlias = newTermName(c.fresh() + "0")

    c.Expr[PathModify[T, U]](Block(
      q"val $valueAlias = $wrappedValue",
      q"_root_.com.softwaremill.quicklens.PathModify(${Ident(valueAlias)}, $modifications)"
    ))
  }

  /**
   * Compose modifications generated for each path, from left to right.
   */
  private def modificationsForPaths[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(
    path1: c.Expr[T => U],
    paths: Seq[c.Expr[T => U]]
  ): c.Tree = {
    import c.universe._

    val valueName = newTermName(c.fresh() + "0")
    val modifierName = newTermName(c.fresh() + "0")

    val modification1 = q"${modificationForPath(c)(path1)}($valueName, $modifierName)"
    val chained = paths.foldLeft(modification1) {
      case (tree, path) =>
        val modification = modificationForPath(c)(path)
        q"$modification($tree, $modifierName)"
    }

    val valueArg = q"val $valueName: ${weakTypeOf[T]}"
    val modifierArg = q"val $modifierName: (${weakTypeOf[U]} => ${weakTypeOf[U]})"
    q"($valueArg, $modifierArg) => $chained"
  }

  /**
   * Produce a modification for a single path.
   */
  private def modificationForPath[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(
    path: c.Expr[T => U]
  ): c.Tree = {
    import c.universe._

    sealed trait PathAccess
    case object DirectPathAccess extends PathAccess
    case class SealedPathAccess(types: Set[Symbol]) extends PathAccess

    sealed trait PathElement
    case class TermPathElement(term: c.TermName, access: PathAccess, xargs: c.Tree*) extends PathElement
    case class FunctorPathElement(functor: c.Tree, method: c.TermName, xargs: c.Tree*) extends PathElement

    /**
     * Determine if the `.copy` method should be applied directly
     * or through a match across all subclasses (for sealed traits).
     */
    def determinePathAccess(typeSymbol: Symbol) = {
      def ifEmpty[A](set: Set[A], empty: => Set[A]) =
        if (set.isEmpty) empty else set

      def knownDirectSubclasses(sealedSymbol: ClassSymbol) = ifEmpty(
        sealedSymbol.knownDirectSubclasses,
        c.abort(
          c.enclosingPosition,
          s"""Could not find subclasses of sealed trait $sealedSymbol.
             |You might need to ensure that it gets compiled before this invocation.
             |See also: <https://issues.scala-lang.org/browse/SI-7046>.""".stripMargin
        )
      )

      def expand(symbol: Symbol): Set[Symbol] = Set(symbol)
        .filter(_.isClass)
        .map(_.asClass)
        .map { s => s.typeSignature; s } // see <https://issues.scala-lang.org/browse/SI-7755>
        .filter(_.isSealed)
        .flatMap(s => knownDirectSubclasses(s))
        .flatMap(s => ifEmpty(expand(s), Set(s)))

      val subclasses = expand(typeSymbol)
      if (subclasses.isEmpty) DirectPathAccess else SealedPathAccess(subclasses)
    }

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
        case q"$parent.$child" =>
          val access = determinePathAccess(parent.tpe.typeSymbol)
          collectPathElements(parent, TermPathElement(child, access) :: acc)
        case q"$parent.$method(..$xargs)" if methodSupported(method) =>
          collectPathElements(parent, TermPathElement(method, DirectPathAccess, xargs:_*) :: acc)
        case q"$tpname[..$x]($t)($f)" if typeSupported(tpname) =>
          val newAcc = acc match {
            // replace the term controlled by quicklens
            case TermPathElement(term, _, xargs @ _*) :: rest => FunctorPathElement(f, term, xargs: _*) :: rest
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
          case TermPathElement(term, _) :: tail => terms(tail, term :: result)
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
     * (tree, DirectPathAccess) => f(tree)
     *
     * (tree, SealedPathAccess(Set(T1, T2, ...)) => tree match {
     *   case x1: T1 => f(x1)
     *   case x2: T2 => f(x2)
     *   ...
     * }
     */
    def generateAccess(tree: c.Tree, access: PathAccess)(f: c.Tree => c.Tree) = access match {
      case DirectPathAccess => f(tree)
      case SealedPathAccess(types) =>
        val cases = types map { tp =>
          val pat = newTermName(c.fresh() + "0")
          cq"$pat: $tp => ${f(Ident(pat))}"
        }
        q"$tree match { case ..$cases }"
    }

    /**
     * (a, List(TPE(d), TPE(c), FPE(functor, method, xargs), TPE(b)), k) =>
     *   (aa, aa.copy(b = functor.method(aa.b, xargs)(a => a.copy(c = a.c.copy(d = k)))
     */
    def generateCopies(rootPathEl: c.TermName, reversePathEls: List[PathElement], newVal: c.Tree): (c.TermName, c.Tree) = {
      reversePathEls match {
        case Nil => (rootPathEl, newVal)
        case TermPathElement(pathEl, access) :: tail =>
          val selectCurrVal = generateSelects(rootPathEl, tail)
          val copy = generateAccess(selectCurrVal, access) { currVal =>
            q"$currVal.copy($pathEl = $newVal)"
          }
          generateCopies(rootPathEl, tail, copy)
        case FunctorPathElement(functor, method, xargs @ _*) :: tail =>
          val newRootPathEl = newTermName(c.fresh() + "0")
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
    val initialRootPathEl = newTermName(c.fresh() + "0")
    val fn = newTermName(c.fresh() + "0") // the function that modifies the last path element

    val reversePathEls = pathEls.reverse

    // new value of the last path element is an invocation of $fn on the current last path element value
    val select = generateSelects(initialRootPathEl, reversePathEls)
    val mod = q"$fn($select)"

    val (rootPathEl, copies) = generateCopies(initialRootPathEl, reversePathEls, mod)

    val rootPathElParamTree = q"val $rootPathEl: ${weakTypeOf[T]}"
    val fnParamTree = q"val $fn: (${weakTypeOf[U]} => ${weakTypeOf[U]})"

    q"($rootPathElParamTree, $fnParamTree) => $copies"
  }
}
