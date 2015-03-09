package com.softwaremill

import scala.language.experimental.macros

package object quicklens {
  case class PathModify[T, U](obj: T, doModify: (T, U => U) => T) {
    def using(mod: U => U): T = doModify(obj, mod)
  }

  implicit class AbstractPathModify[T, U](f1: T => PathModify[T, U]) {
    def andThenModify[V](f2: U => PathModify[U, V]): T => PathModify[T, V] = { (t: T) =>
      PathModify[T, V](t, (t, vv) => f1(t).doModify(t, u => f2(u).doModify(u, vv)))
    }
  }

  def modify[T, U](obj: T)(path: T => U): PathModify[T, U] = macro QuicklensMacros.modify_impl[T, U]
}
