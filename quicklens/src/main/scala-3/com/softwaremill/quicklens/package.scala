package com.softwaremill

import scala.collection.{Factory, SortedMap}
import scala.annotation.compileTimeOnly
import com.softwaremill.quicklens.QuicklensMacros._
import scala.reflect.ClassTag

package object quicklens {

  // #114: obj shouldn't be inline since we want to reference the parameter by-name, rather then embedding the whole
  // expression whenever obj is used; this is especially important for chained .modify invocations
  extension [S](obj: S)
    /** Create an object allowing modifying the given (deeply nested) field accessible in a `case class` hierarchy via
      * `path` on the given `obj`.
      *
      * All modifications are side-effect free and create copies of the original objects.
      *
      * You can use `.each` to traverse options, lists, etc.
      */
    inline def modify[A](inline path: S => A): PathModify[S, A] = ${ toPathModifyFromFocus('obj, 'path) }

    /** Create an object allowing modifying the given (deeply nested) fields accessible in a `case class` hierarchy via
      * `paths` on the given `obj`.
      *
      * All modifications are side-effect free and create copies of the original objects.
      *
      * You can use `.each` to traverse options, lists, etc.
      */
    inline def modifyAll[A](inline path: S => A, inline paths: (S => A)*): PathModify[S, A] = ${
      modifyAllImpl('obj, 'path, 'paths)
    }

  case class PathModify[S, A](obj: S, f: (A => A) => S) {

    /** Transform the value of the field(s) using the given function.
      *
      * @return
      *   A copy of the root object with the (deeply nested) field(s) modified.
      */
    def using(mod: A => A): S = f.apply(mod)

    /** An alias for [[using]]. Explicit calls to [[using]] are preferred over this alias, but quicklens provides this
      * option because code auto-formatters (like scalafmt) will generally not keep [[modify]]/[[using]] pairs on the
      * same line, leading to code like
      * {{{
      * x
      *   .modify(_.foo)
      *   .using(newFoo :: _)
      *   .modify(_.bar)
      *   .using(_ + newBar)
      * }}}
      * When using [[apply]], scalafmt will allow
      * {{{
      * x
      *   .modify(_.foo)(newFoo :: _)
      *   .modify(_.bar)(_ + newBar)
      * }}}
      */
    def apply(mod: A => A): S = using(mod)

    /** Transform the value of the field(s) using the given function, if the condition is true. Otherwise, returns the
      * original object unchanged.
      *
      * @return
      *   A copy of the root object with the (deeply nested) field(s) modified, if `condition` is true.
      */
    def usingIf(condition: Boolean)(mod: A => A): S = if condition then using(mod) else obj

    /** Set the value of the field(s) to a new value.
      *
      * @return
      *   A copy of the root object with the (deeply nested) field(s) set to the new value.
      */
    def setTo(v: A): S = f.apply(Function.const(v))

    /** Set the value of the field(s) to a new value, if it is defined. Otherwise, returns the original object
      * unchanged.
      *
      * @return
      *   A copy of the root object with the (deeply nested) field(s) set to the new value, if it is defined.
      */
    def setToIfDefined(v: Option[A]): S = v.fold(obj)(setTo)

    /** Set the value of the field(s) to a new value, if the condition is true. Otherwise, returns the original object
      * unchanged.
      *
      * @return
      *   A copy of the root object with the (deeply nested) field(s) set to the new value, if `condition` is true.
      */
    def setToIf(condition: Boolean)(v: A): S = if condition then setTo(v) else obj
  }

  def modifyLens[T]: LensHelper[T] = LensHelper[T]()
  def modifyAllLens[T]: MultiLensHelper[T] = MultiLensHelper[T]()

  case class LensHelper[T] private[quicklens] () {
    inline def apply[U](inline path: T => U): PathLazyModify[T, U] = ${ modifyLensApplyImpl('path) }
  }

  case class MultiLensHelper[T] private[quicklens] () {
    inline def apply[U](inline path1: T => U, inline paths: (T => U)*): PathLazyModify[T, U] = ${
      modifyAllLensApplyImpl('path1, 'paths)
    }
  }

  case class PathLazyModify[T, U](doModify: (T, U => U) => T) { self =>

    /** see [[PathModify.using]] */
    def using(mod: U => U): T => T = obj => doModify(obj, mod)

    /** see [[PathModify.usingIf]] */
    def usingIf(condition: Boolean)(mod: U => U): T => T =
      obj =>
        if (condition) doModify(obj, mod)
        else obj

    /** see [[PathModify.setTo]] */
    def setTo(v: U): T => T = obj => doModify(obj, _ => v)

    /** see [[PathModify.setToIfDefined]] */
    def setToIfDefined(v: Option[U]): T => T = v.fold((obj: T) => obj)(setTo)

    /** see [[PathModify.setToIf]] */
    def setToIf(condition: Boolean)(v: => U): T => T =
      if (condition) setTo(v)
      else obj => obj

    def andThenModify[V](f2: PathLazyModify[U, V]): PathLazyModify[T, V] =
      PathLazyModify[T, V]((t, vv) => self.doModify(t, u => f2.doModify(u, vv)))
  }

  trait QuicklensFunctor[F[_]] {
    def map[A](fa: F[A], f: A => A): F[A]
    def each[A](fa: F[A], f: A => A): F[A] = map(fa, f)
    def eachWhere[A](fa: F[A], f: A => A, cond: A => Boolean): F[A] = map(fa, x => if cond(x) then f(x) else x)
  }

  object QuicklensFunctor {
    given [S <: ([V] =>> Seq[V])]: QuicklensFunctor[S] with {
      def map[A](fa: S[A], f: A => A): S[A] = fa.map(f).asInstanceOf[S[A]]
    }

    given QuicklensFunctor[Option] with {
      def map[A](fa: Option[A], f: A => A): Option[A] = fa.map(f)
    }

    given QuicklensFunctor[Array] with {
      def map[A](fa: Array[A], f: A => A): Array[A] =
        implicit val aClassTag: ClassTag[A] = fa.elemTag.asInstanceOf[ClassTag[A]]
        fa.map(f)
    }

    given [K, M <: ([V] =>> Map[K, V])]: QuicklensFunctor[M] with {
      def map[A](fa: M[A], f: A => A): M[A] = {
        val mapped = fa.view.mapValues(f)
        (fa match {
          case sfa: SortedMap[K, A] => sfa.sortedMapFactory.from(mapped)(using sfa.ordering)
          case _                    => mapped.to(fa.mapFactory)
        }).asInstanceOf[M[A]]
      }
    }
  }

  trait QuicklensIndexedFunctor[F[_], -I] {
    def at[A](fa: F[A], f: A => A, idx: I): F[A]
    def atOrElse[A](fa: F[A], f: A => A, idx: I, default: => A): F[A]
    def index[A](fa: F[A], f: A => A, idx: I): F[A]
  }

  object QuicklensIndexedFunctor {
    given [S <: ([V] =>> Seq[V])]: QuicklensIndexedFunctor[S, Int] with {
      def at[A](fa: S[A], f: A => A, idx: Int): S[A] =
        fa.updated(idx, f(fa(idx))).asInstanceOf[S[A]]
      def atOrElse[A](fa: S[A], f: A => A, idx: Int, default: => A): S[A] =
        fa.updated(idx, f(fa.applyOrElse(idx, Function.const(default)))).asInstanceOf[S[A]]
      def index[A](fa: S[A], f: A => A, idx: Int): S[A] =
        if fa.isDefinedAt(idx) then fa.updated(idx, f(fa(idx))).asInstanceOf[S[A]] else fa
    }

    given QuicklensIndexedFunctor[Array, Int] with {
      def at[A](fa: Array[A], f: A => A, idx: Int): Array[A] =
        implicit val aClassTag: ClassTag[A] = fa.elemTag.asInstanceOf[ClassTag[A]]
        fa.updated(idx, f(fa(idx)))
      def atOrElse[A](fa: Array[A], f: A => A, idx: Int, default: => A): Array[A] =
        implicit val aClassTag: ClassTag[A] = fa.elemTag.asInstanceOf[ClassTag[A]]
        fa.updated(idx, f(fa.applyOrElse(idx, Function.const(default))))
      def index[A](fa: Array[A], f: A => A, idx: Int): Array[A] =
        implicit val aClassTag: ClassTag[A] = fa.elemTag.asInstanceOf[ClassTag[A]]
        if fa.isDefinedAt(idx) then fa.updated(idx, f(fa(idx))) else fa
    }

    given [K, M <: ([V] =>> Map[K, V])]: QuicklensIndexedFunctor[M, K] with {
      def at[A](fa: M[A], f: A => A, idx: K): M[A] =
        fa.updated(idx, f(fa(idx))).asInstanceOf[M[A]]
      def atOrElse[A](fa: M[A], f: A => A, idx: K, default: => A): M[A] =
        fa.updated(idx, f(fa.applyOrElse(idx, _ => default))).asInstanceOf[M[A]]
      def index[A](fa: M[A], f: A => A, idx: K): M[A] =
        if fa.isDefinedAt(idx) then fa.updated(idx, f(fa(idx))).asInstanceOf[M[A]] else fa
    }
  }

  trait QuicklensEitherFunctor[T[_, _], L, R]:
    def eachLeft[A](e: T[A, R], f: A => A): T[A, R]
    def eachRight[A](e: T[L, A], f: A => A): T[L, A]

  object QuicklensEitherFunctor:
    given [L, R]: QuicklensEitherFunctor[Either, L, R] with
      override def eachLeft[A](e: Either[A, R], f: A => A) = e.left.map(f)
      override def eachRight[A](e: Either[L, A], f: A => A) = e.map(f)

  // Currently only used for [[Option]], but could be used for [[Right]]-biased [[Either]]s.
  trait QuicklensSingleAtFunctor[F[_]]:
    def at[A](fa: F[A], f: A => A): F[A]
    def atOrElse[A](fa: F[A], f: A => A, default: => A): F[A]
    def index[A](fa: F[A], f: A => A): F[A]

  object QuicklensSingleAtFunctor:
    given QuicklensSingleAtFunctor[Option] with
      override def at[A](fa: Option[A], f: A => A): Option[A] = Some(fa.map(f).get)
      override def atOrElse[A](fa: Option[A], f: A => A, default: => A): Option[A] = fa.orElse(Some(default)).map(f)
      override def index[A](fa: Option[A], f: A => A): Option[A] = fa.map(f)

  trait QuicklensWhen[A]:
    inline def when[B <: A](a: A, f: B => B): A

  object QuicklensWhen:
    given [A]: QuicklensWhen[A] with
      override inline def when[B <: A](a: A, f: B => B): A =
        a match
          case b: B => f(b)
          case _    => a

  extension [F[_]: QuicklensFunctor, A](fa: F[A])
    @compileTimeOnly(canOnlyBeUsedInsideModify("each"))
    def each: A = ???

    @compileTimeOnly(canOnlyBeUsedInsideModify("eachWhere"))
    def eachWhere(cond: A => Boolean): A = ???

  extension [F[_]: ([G[_]] =>> QuicklensIndexedFunctor[G, I]), I, A](fa: F[A])
    @compileTimeOnly(canOnlyBeUsedInsideModify("at"))
    def at(idx: I): A = ???

    @compileTimeOnly(canOnlyBeUsedInsideModify("atOrElse"))
    def atOrElse(idx: I, default: => A): A = ???

    @compileTimeOnly(canOnlyBeUsedInsideModify("index"))
    def index(idx: I): A = ???

  extension [T[_, _]: ([E[_, _]] =>> QuicklensEitherFunctor[E, L, R]), R, L](e: T[L, R])
    @compileTimeOnly(canOnlyBeUsedInsideModify("eachLeft"))
    def eachLeft: L = ???

  extension [T[_, _]: ([E[_, _]] =>> QuicklensEitherFunctor[E, L, R]), L, R](e: T[L, R])
    @compileTimeOnly(canOnlyBeUsedInsideModify("eachRight"))
    def eachRight: R = ???

  extension [F[_]: QuicklensSingleAtFunctor, T](t: F[T])
    @compileTimeOnly(canOnlyBeUsedInsideModify("at"))
    def at: T = ???

    @compileTimeOnly(canOnlyBeUsedInsideModify("atOrElse"))
    def atOrElse(default: => T): T = ???

    @compileTimeOnly(canOnlyBeUsedInsideModify("index"))
    def index: T = ???

  extension [A: QuicklensWhen](value: A)
    @compileTimeOnly(canOnlyBeUsedInsideModify("when"))
    def when[B <: A]: B = ???

  extension [T, U](f1: T => PathModify[T, U])
    def andThenModify[V](f2: U => PathModify[U, V]): T => PathModify[T, V] = (t: T) =>
      PathModify[T, V](t, vv => f1(t).f(u => f2(u).f(vv)))

  private def canOnlyBeUsedInsideModify(method: String) = s"$method can only be used as a path component inside modify"
}
