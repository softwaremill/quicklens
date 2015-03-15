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
   */
  def modify[T, U](obj: T)(path: T => U): PathModify[T, U] = macro QuicklensMacros.modify_impl[T, U]

  case class PathModify[T, U](obj: T, doModify: (T, U => U) => T) {
    /**
     * Transform the value of the field using the given function.
     * @return A copy of the root object with the (deeply nested) field modified.
     */
    def using(mod: U => U): T = doModify(obj, mod)
    /**
     * Set the value of the field to a new value.
     * @return A copy of the root object with the (deeply nested) field set to the new value.
     */
    def setTo(v: U): T = doModify(obj, _ => v)
  }

  implicit class AbstractPathModify[T, U](f1: T => PathModify[T, U]) {
    def andThenModify[V](f2: U => PathModify[U, V]): T => PathModify[T, V] = { (t: T) =>
      PathModify[T, V](t, (t, vv) => f1(t).doModify(t, u => f2(u).doModify(u, vv)))
    }
  }

  implicit class QuicklensEach[F[_], T](t: F[T])(implicit f: QuicklensFunctor[F, T, T]) {
    def each: T = sys.error("Can only be used inside modify!")
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
