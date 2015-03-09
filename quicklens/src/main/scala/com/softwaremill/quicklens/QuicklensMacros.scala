package com.softwaremill.quicklens

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

object QuicklensMacros {
  /**
   * modify(a)(_.b.c) => new PathMod(a, (A, F) => A.copy(b = A.b.copy(c = F(A.b.c))))
   */
  def modify_impl[T, U](c: blackbox.Context)(obj: c.Expr[T])(path: c.Expr[T => U]): c.Tree = {
    import c.universe._

    val pathEls = path.tree match {
      case q"($arg) => $pathBody" => collectPathElements(c)(pathBody, Nil)
      case _ => c.abort(c.enclosingPosition, "Path must have shape: _.field1.field2.(...), got: " + path.tree)
    }

    val rootPathEl = TermName(c.freshName()) // the root object (same as obj)
    val fn = TermName(c.freshName()) // the function that modifies that last path element

    // new value of the last path element is an invocation of $fn on the current last path element value
    val select = generateSelects(c)(rootPathEl, pathEls)
    val mod = q"$fn($select)"

    val copies = generateCopies(c)(rootPathEl, pathEls.reverse, mod)

    val rootPathElParamTree = ValDef(Modifiers(), rootPathEl, TypeTree(), EmptyTree)
    val fnParamTree = ValDef(Modifiers(), fn, TypeTree(), EmptyTree)
    q"com.softwaremill.quicklens.PathModify($obj, ($rootPathElParamTree, $fnParamTree) => $copies)"
  }

  /**
   * _.a.b.c => List(a, b, c)
   */
  @tailrec
  private def collectPathElements(c: blackbox.Context)(tree: c.Tree, acc: List[c.TermName]): List[c.TermName] = {
    import c.universe._

    tree match {
      case q"$parent.$child" => collectPathElements(c)(parent, child :: acc)
      case t: Ident => acc
      case _ => c.abort(c.enclosingPosition, "Unsupported path element: " + tree)
    }
  }

  /**
   * (a, List(d, c, b), k) => a.copy(b = a.b.copy(c = a.b.c.copy(d = k))
   */
  private def generateCopies(c: blackbox.Context)(rootPathEl: c.TermName, reversePathEls: List[c.TermName], newVal: c.Tree): c.Tree = {
    import c.universe._

    reversePathEls match {
      case Nil => newVal
      case pathEl :: tail =>
        val selectCopy = generateSelects(c)(rootPathEl, (TermName("copy") :: tail).reverse)
        val copy = q"$selectCopy($pathEl = $newVal)"
        generateCopies(c)(rootPathEl, tail, copy)
    }
  }

  /**
   * (x, List(a, b, c)) => x.a.b.c
   */
  private def generateSelects(c: blackbox.Context)(rootPathEl: c.TermName, pathEls: List[c.TermName]): c.Tree = {
    import c.universe._

    @tailrec
    def go(els: List[c.TermName], result: c.Tree): c.Tree = {
      els match {
        case Nil => result
        case pathEl :: tail =>
          val select = q"$result.$pathEl"
          go(tail, select)
      }
    }

    go(pathEls, Ident(rootPathEl))
  }
}
