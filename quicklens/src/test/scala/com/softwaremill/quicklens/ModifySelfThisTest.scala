package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ModifySelfThisTest._

object ModifySelfThisTest {

  case class State(x: Int) { self =>

    def mod: State = this.modify(_.x).setTo(1)
  }

  trait A {
    def a: Unit
  }

  case class State1(x: Int) extends A { self: A =>

    def mod: State1 = this.modify(_.x).setTo(1)

    def a: Unit = ()
  }
}

class ModifySelfThisTest extends AnyFlatSpec with Matchers {
  it should "modify an object even in presence of self alias" in {
    val s = State(0)
    val modified = s.mod

    modified.x shouldBe 1
  }

  it should "modify an object even in presence of self type" in {
    val s = State(0)
    val modified = s.mod

    modified.x shouldBe 1
  }
}
