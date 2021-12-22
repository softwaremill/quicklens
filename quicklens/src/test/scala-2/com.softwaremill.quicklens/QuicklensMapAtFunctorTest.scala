package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuicklensMapAtFunctorTest extends AnyFlatSpec with Matchers {

  "QuicklensMapAtFunctor" should "work for types which is not a subtype of Map" in {
    case class MapLike[K, V](underlying: Map[K, V])

    implicit def instance[K, T]: QuicklensMapAtFunctor[MapLike, K, T] = new QuicklensMapAtFunctor[MapLike, K, T] {
      private val mapInstance: QuicklensMapAtFunctor[Map, K, T] =
        implicitly[QuicklensMapAtFunctor[Map, K, T]]

      def at(fa: MapLike[K, T], idx: K)(f: T => T): MapLike[K, T] =
        MapLike(mapInstance.at(fa.underlying, idx)(f))
      def atOrElse(fa: MapLike[K, T], idx: K, default: => T)(f: T => T): MapLike[K, T] =
        MapLike(mapInstance.atOrElse(fa.underlying, idx, default)(f))
      def index(fa: MapLike[K, T], idx: K)(f: T => T): MapLike[K, T] =
        MapLike(mapInstance.index(fa.underlying, idx)(f))
      def each(fa: MapLike[K, T])(f: T => T): MapLike[K, T] =
        MapLike(mapInstance.each(fa.underlying)(f))
    }

    modify(MapLike(m1))(_.at("K1").a5.name).using(duplicate) should be(MapLike(m1dup))
  }
}
