package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExplicitCopyTest extends AnyFlatSpec with Matchers {
  it should "modify a class with an explicit copy method" in {
    case class V(x: Double, y: Double)
    class Vec(val v: V) {
      def x: Double = v.x
      def y: Double = v.y
      def copy(x: Double = v.x, y: Double = v.y): Vec = new Vec(V(x, y)) {}
      def show: String = s"Vec(${v.x}, ${v.y})"
    }
    object Vec {
      def apply(x: Double, y: Double): Vec = new Vec(V(x, y)) {}
    }

    val vec = Vec(1, 2)
    val modified = vec.modify(_.x).using(_ + 1)
    val expected = Vec(2, 2)
    modified.show shouldEqual expected.show
  }

  it should "modify a class that has a method with the same name as a field" in {
    final case class PathItem()
    final case class Paths(
      pathItems: Map[String, PathItem] = Map.empty
    )
    final case class Docs(
      paths: Paths = Paths()
    ) {
      def paths(paths: Paths): Docs = copy(paths = paths)
    }
    val docs = Docs()
    docs.modify(_.paths.pathItems).using(m => m + ("a" -> PathItem()))
  }

}
