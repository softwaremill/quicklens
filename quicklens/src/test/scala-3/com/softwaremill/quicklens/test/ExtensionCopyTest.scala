package com.softwaremill.quicklens
package test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object ExtensionCopyTest {
  case class V(x: Double, y: Double)

  opaque type Vec = V

  object Vec {
    def apply(x: Double, y: Double): Vec = V(x, y)
  }

  extension (v: Vec) {
    def x: Double = v.x
    def y: Double = v.y
    def copy(x: Double = v.x, y: Double = v.y): Vec = V(x, y)
  }
}

class ExtensionCopyTest extends AnyFlatSpec with Matchers {
  it should "modify a class with an extension copy method" in {
    case class V(x: Double, y: Double)

    class Vec(val v: V)

    object Vec {
      def apply(x: Double, y: Double): Vec = new Vec(V(x, y))
    }

    extension (v: Vec) {
      def x: Double = v.v.x
      def y: Double = v.v.y
      def copy(x: Double = v.x, y: Double = v.y): Vec = new Vec(V(x, y))
    }
    val a = Vec(1, 2)
    val b = a.modify(_.x).using(_ + 1)
    println(b)
  }

  it should "modify an opaque type with an extension copy method" in {
    import ExtensionCopyTest.*

    val a = Vec(1, 2)
    val b = a.modify(_.x).using(_ + 1)
    println(b)
  }
}
