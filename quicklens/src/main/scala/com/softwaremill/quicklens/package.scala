package com.softwaremill

import scala.language.experimental.macros

package object quicklens {
  def modify[T, U](obj: T)(path: T => U): PathModify[T, U] = macro QuicklensMacros.modify_impl[T, U]

  case class PathModify[T, U](obj: T, doModify: (T, U => U) => T) {
    def using(mod: U => U): T = doModify(obj, mod)
    def setTo(v: U): T = doModify(obj, _ => v)
  }

  implicit class AbstractPathModify[T, U](f1: T => PathModify[T, U]) {
    def andThenModify[V](f2: U => PathModify[U, V]): T => PathModify[T, V] = { (t: T) =>
      PathModify[T, V](t, (t, vv) => f1(t).doModify(t, u => f2(u).doModify(u, vv)))
    }
  }

  implicit class QuicklensEach[F[_], T](t: F[T])(implicit f: QuicklensFunctor[F]) {
    def each: T = sys.error("Cano only be used inside modify!")
  }

  trait QuicklensFunctor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }
  implicit object OptionQuicklensFunctor extends QuicklensFunctor[Option] {
    override def map[A, B](fa: Option[A])(f: (A) => B) = fa.map(f)
  }
  implicit object ListQuicklensFunctor extends QuicklensFunctor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B) = fa.map(f)
  }
  implicit object VectorQuicklensFunctor extends QuicklensFunctor[Vector] {
    override def map[A, B](fa: Vector[A])(f: (A) => B) = fa.map(f)
  }
}
