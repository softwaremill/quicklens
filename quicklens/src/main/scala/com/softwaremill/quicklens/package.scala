package com.softwaremill

import scala.annotation.compileTimeOnly
import scala.collection.TraversableLike
import scala.collection.SeqLike
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

  implicit class AbstractPathModifyPimp[T, U](f1: T => PathModify[T, U]) {
    def andThenModify[V](f2: U => PathModify[U, V]): T => PathModify[T, V] = { (t: T) =>
      PathModify[T, V](t, (t, vv) => f1(t).doModify(t, u => f2(u).doModify(u, vv)))
    }
  }

  implicit class QuicklensEach[F[_], T](t: F[T])(implicit f: QuicklensFunctor[F, T, T]) {
    @compileTimeOnly("each can only be used inside modify")
    def each: T = sys.error("")
  }

  trait QuicklensFunctor[F[_], A, B] {
    def map(fa: F[A])(f: A => B): F[B]
    def each(fa: F[A])(f: A => B): F[B] = map(fa)(f)
  }

  implicit def optionQuicklensFunctor[A, B]: QuicklensFunctor[Option, A, B] =
    new QuicklensFunctor[Option, A, B] {
      override def map(fa: Option[A])(f: A => B) = fa.map(f)
    }

  implicit def traversableQuicklensFunctor[F[_], A, B](implicit cbf: CanBuildFrom[F[A], B, F[B]], ev: F[A] => TraversableLike[A, F[A]]) =
    new QuicklensFunctor[F, A, B] {
      override def map(fa: F[A])(f: A => B) = fa.map(f)
    }

  implicit class QuicklensAt[F[_], T](t: F[T])(implicit f: QuicklensAtFunctor[F, T]) {
    @compileTimeOnly("at can only be used inside modify")
    def at(idx: Int): T = sys.error("")
  }

  trait QuicklensAtFunctor[F[_], T] {
    def at(fa: F[T], idx: Int)(f: T => T): F[T]
  }

  implicit def seqQuicklensFunctor[F[_], T](implicit cbf: CanBuildFrom[F[T], T, F[T]], ev: F[T] => SeqLike[T, F[T]]) =
    new QuicklensAtFunctor[F, T] {
      override def at(fa: F[T], idx: Int)(f: T => T) = {
        val builder = cbf(fa)

        if (idx >= fa.length) {
          throw new IndexOutOfBoundsException(idx.toString)
        }

        var i = 0
        fa.foreach { e =>
          if (i == idx) builder += f(fa(idx))
          else builder += fa(i)

          i += 1
        }

        builder.result
      }
    }
}
