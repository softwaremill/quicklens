package com.softwaremill.quicklens

import scala.annotation.compileTimeOnly

private[quicklens] trait LowPriorityImplicits {

  /**
   * `QuicklensEach` is in `LowPriorityImplicits` to not conflict with the `QuicklensMapAtFunctor` on `each` calls.
   */
  implicit class QuicklensEach[F[_], T](t: F[T])(implicit f: QuicklensFunctor[F, T]) {
    @compileTimeOnly(canOnlyBeUsedInsideModify("each"))
    def each: T = sys.error("")

    @compileTimeOnly(canOnlyBeUsedInsideModify("eachWhere"))
    def eachWhere(p: T => Boolean): T = sys.error("")
  }
}
