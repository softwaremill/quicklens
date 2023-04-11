package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SecondParamListTest extends AnyFlatSpec with Matchers {
  it should "modify an object with second implicit param list" in {
    import com.softwaremill.quicklens._

    case class State(inside: Boolean)(implicit d: Double)

    val d: Double = 1.0

    val state1 = State(true)(d)

    implicit val dd: Double = d
    val state2 = state1.modify(_.inside).setTo(true)

    state1 should be(state2)
  }

  it should "should give a meaningful error for an object with more than one non-implicit param list" in {
    import com.softwaremill.quicklens._

    case class State(inside: Boolean)(d: Double)

    val d: Double = 1.0

    val state1 = State(true)(d)

    implicit val dd: Double = d

    assertDoesNotCompile("state1.modify(_.inside).setTo(true)")
  }
}
