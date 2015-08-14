package com.softwaremill

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.language.experimental.macros

package object quicklens {
  /**
   * Create an object allowing modifying the given (deeply nested) field accessible in a `case class` hierarchy
   * via `path` on the given `obj`.
   *
   * All modifications are side-effect free and create copies of the original objects.
   *
   * You can use `.each` to traverse options, lists, etc.
   */
  def modify[T, U](obj: T)(path: T => U): PathModify[T, U] = macro QuicklensMacros.modify_impl[T, U]

  implicit class ModifyPimp[T](t: T) {
    /**
     * Create an object allowing modifying the given (deeply nested) field accessible in a `case class` hierarchy
     * via `path` on the given `obj`.
     *
     * All modifications are side-effect free and create copies of the original objects.
     *
     * You can use `.each` to traverse options, lists, etc.
     */
    def modify[U](path: T => U): PathModify[T, U] = macro QuicklensMacros.modifyPimp_impl[T, U]
  }

  implicit class AbstractPathModifyPimp[T, U](f1: T => PathModify[T, U]) {
    def andThenModify[V](f2: U => PathModify[U, V]): T => PathModify[T, V] = { (t: T) =>
      PathModify[T, V](t, (t, vv) => f1(t).doModify(t, u => f2(u).doModify(u, vv)))
    }
  }

  implicit class QuicklensEach[F[_], T](t: F[T])(implicit f: QuicklensFunctor[F, T, T]) {
    def each: T = sys.error("")
  }

  trait QuicklensFunctor[F[_], A, B] {
    def map(fa: F[A])(f: A => B): F[B]
  }
  implicit def optionQuicklensFunctor[A, B]: QuicklensFunctor[Option, A, B] =
    new QuicklensFunctor[Option, A, B] {
      override def map(fa: Option[A])(f: (A) => B) = fa.map(f)
    }
  implicit def traversableQuicklensFunctor[F[_], A, B](implicit cbf: CanBuildFrom[F[A], B, F[B]], e1: F[A] => TraversableLike[A, F[A]]): QuicklensFunctor[F, A, B] =
    new QuicklensFunctor[F, A, B] {
      override def map(fa: F[A])(f: (A) => B) = fa.map(f)
    }
}
