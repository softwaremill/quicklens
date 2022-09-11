package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RepeatedModifyTest extends AnyFlatSpec with Matchers {
  import RepeatedModifyTest._

  it should "properly handle repeated modify invocations for different fields" in {
    val c = C(B(1, 1, 1, 1, 1))
    c
      .modify(_.b.a1)
      .setTo(0.0d)
      .modify(_.b.a2)
      .setTo(0.0d)
      .modify(_.b.a3)
      .setTo(0.0d)
      .modify(_.b.a4)
      .setTo(0.0d)
      .modify(_.b.a5)
      .setTo(0.0d) shouldBe C(B(0, 0, 0, 0, 0))
  }

  it should "properly handle repeated modify invocations for the same field" in {
    val c = C(B(1, 1, 1, 1, 1))
    c
      .modify(_.b.a1)
      .setTo(0.0d)
      .modify(_.b.a1)
      .setTo(1.0d)
      .modify(_.b.a1)
      .setTo(2.0d)
      .modify(_.b.a1)
      .setTo(3.0d)
      .modify(_.b.a1)
      .setTo(4.0d) shouldBe C(B(4, 1, 1, 1, 1))
  }
}

object RepeatedModifyTest {
  case class B(a1: Double, a2: Double, a3: Double, a4: Double, a5: Double)
  case class C(b: B)
}
