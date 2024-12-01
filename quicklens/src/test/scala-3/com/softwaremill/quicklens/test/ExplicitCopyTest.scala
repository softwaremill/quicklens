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

  it should "modify a case class with an additional explicit copy" in {
    case class Frozen(state: String, ext: Int) {
      def copy(stateC: Char): Frozen = Frozen(stateC.toString, ext)
    }

    val f = Frozen("A", 0)
    f.modify(_.state).setTo("B")
  }

  it should "modify a case class with an ambiguous additional explicit copy" in {
    case class Frozen(state: String, ext: Int) {
      def copy(state: String): Frozen = Frozen(state, ext)
    }

    val f = Frozen("A", 0)
    f.modify(_.state).setTo("B")
  }

  it should "modify a class with two explicit copy methods" in {
    class Frozen(val state: String, val ext: Int) {
      def copy(state: String = state, ext: Int = ext): Frozen = new Frozen(state, ext)
      def copy(state: String): Frozen = new Frozen(state, ext)
    }

    val f = new Frozen("A", 0)
    f.modify(_.state).setTo("B")
  }

  it should "modify a case class with an ambiguous additional explicit copy and pick the synthetic one first" in {
    var accessed = 0
    case class Frozen(state: String, ext: Int) {
      def copy(state: String): Frozen =
        accessed += 1
        Frozen(state, ext)
    }

    val f = Frozen("A", 0)
    f.modify(_.state).setTo("B")
    accessed shouldEqual 0
  }

  it should "not compile when modifying a field which is not present as a copy parameter" in {
    """
    case class Content(x: String)

    class A(val c: Content) {
      def copy(x: String = c.x): A = new A(Content(x))
    }

    val a = new A(Content("A"))
    val am = a.modify(_.c).setTo(Content("B"))
    """ shouldNot compile
  }

  // TODO: Would be nice to be able to handle this case. Based on the types, it
  // is obvious, that the explicit copy should be picked, but I'm not sure if we
  // can get that information

  // it should "pick the correct copy method, based on the type" in {
  //   case class Frozen(state: String, ext: Int) {
  //     def copy(state: Char): Frozen =
  //       Frozen(state.toString, ext)
  //   }

  //   val f = Frozen("A", 0)
  //   f.modify(_.state).setTo('B')
  // }

}
