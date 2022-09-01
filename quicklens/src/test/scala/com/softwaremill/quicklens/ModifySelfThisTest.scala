package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ModifySelfThisTest._

object ModifySelfThisTest {

  case class State(x: Int) {self =>

    def mod: State = this.modify(_.x).setTo(1)
  }

}

class ModifySelfThisTest extends AnyFlatSpec with Matchers {
  it should "modify an object even in presence of self alias" in {
    val s = State(0)
    val modified = s.mod

    modified.x shouldBe 1
  }
}
