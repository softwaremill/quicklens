package com.softwaremill

import scala.annotation.compileTimeOnly
import scala.collection.TraversableLike
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.language.experimental.macros
import scala.language.higherKinds

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

  /**
   * Create an object allowing modifying the given (deeply nested) fields accessible in a `case class` hierarchy
   * via `paths` on the given `obj`.
   *
   * All modifications are side-effect free and create copies of the original objects.
   *
   * You can use `.each` to traverse options, lists, etc.
   */
  def modifyAll[T, U](obj: T)(path1: T => U, paths: (T => U)*): PathModify[T, U] = macro QuicklensMacros.modifyAll_impl[T, U]

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

    /**
     * Create an object allowing modifying the given (deeply nested) fields accessible in a `case class` hierarchy
     * via `paths` on the given `obj`.
     *
     * All modifications are side-effect free and create copies of the original objects.
     *
     * You can use `.each` to traverse options, lists, etc.
     */
    def modifyAll[U](path1: T => U, paths: (T => U)*): PathModify[T, U] = macro QuicklensMacros.modifyAllPimp_impl[T, U]
  }

  case class PathModify[T, U](obj: T, doModify: (T, U => U) => T) {
    /**
     * Transform the value of the field(s) using the given function.
     * @return A copy of the root object with the (deeply nested) field(s) modified.
     */
    def using(mod: U => U): T = doModify(obj, mod)
    /**
     * Set the value of the field(s) to a new value.
     * @return A copy of the root object with the (deeply nested) field(s) set to the new value.
     */
    def setTo(v: U): T = doModify(obj, _ => v)
  }

  implicit class AbstractPathModifyPimp[T, U](f1: T => PathModify[T, U]) {
    def andThenModify[V](f2: U => PathModify[U, V]): T => PathModify[T, V] = { (t: T) =>
      PathModify[T, V](t, (t, vv) => f1(t).doModify(t, u => f2(u).doModify(u, vv)))
    }
  }

  implicit class QuicklensEach[F[_], T](t: F[T])(implicit f: QuicklensFunctor[F, T]) {
    @compileTimeOnly("each can only be used inside modify")
    def each: T = sys.error("")

    @compileTimeOnly("eachWhere can only be used inside modify")
    def eachWhere(p: T => Boolean): T = sys.error("")
  }

  trait QuicklensFunctor[F[_], A] {
    def map(fa: F[A])(f: A => A): F[A]
    def each(fa: F[A])(f: A => A): F[A] = map(fa)(f)
    def eachWhere(fa: F[A], p: A => Boolean)(f: A => A): F[A] = map(fa) { a => if (p(a)) f(a) else a }
  }

  implicit def optionQuicklensFunctor[A]: QuicklensFunctor[Option, A] =
    new QuicklensFunctor[Option, A] {
      override def map(fa: Option[A])(f: A => A) = fa.map(f)
    }

  implicit def traversableQuicklensFunctor[F[_], A](implicit cbf: CanBuildFrom[F[A], A, F[A]], ev: F[A] => TraversableLike[A, F[A]]) =
    new QuicklensFunctor[F, A] {
      override def map(fa: F[A])(f: A => A) = fa.map(f)
    }

  implicit class QuicklensAt[F[_], T](t: F[T])(implicit f: QuicklensAtFunctor[F, T]) {
    @compileTimeOnly("at can only be used inside modify")
    def at(idx: Int): T = sys.error("")
  }

  trait QuicklensAtFunctor[F[_], T] {
    def at(fa: F[T], idx: Int)(f: T => T): F[T]
  }

  implicit class QuicklensMapAt[M[KT, TT] <: Map[KT, TT], K, T](t: M[K, T])(implicit f: QuicklensMapAtFunctor[M, K, T]) {
    @compileTimeOnly("at can only be used inside modify")
    def at(idx: K): T = sys.error("")

    @compileTimeOnly("each can only be used inside modify")
    def each: T = sys.error("")
  }

  trait QuicklensMapAtFunctor[F[_, _], K, T] {
    def at(fa: F[K, T], idx: K)(f: T => T): F[K, T]
    def each(fa: F[K, T])(f: T => T): F[K, T]
  }

  implicit def mapQuicklensFunctor[M[KT, TT] <: Map[KT, TT], K, T](implicit cbf: CanBuildFrom[M[K, T], (K, T), M[K, T]]): QuicklensMapAtFunctor[M, K, T] = new QuicklensMapAtFunctor[M, K, T] {
    override def at(fa: M[K, T], key: K)(f: T => T) = {
      fa.updated(key, f(fa(key))).asInstanceOf[M[K, T]]
    }
    override def each(fa: M[K, T])(f: (T) => T) = {
      val builder = cbf(fa)
      fa.foreach { case(k, t) => builder += k -> f(t) }
      builder.result
    }
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
