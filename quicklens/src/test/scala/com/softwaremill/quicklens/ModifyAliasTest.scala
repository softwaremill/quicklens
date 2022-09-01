package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ModifyAliasTest._

object ModifyAliasTest {

  case class State(x: Int)

  type S = State
}

class ModifyAliasTest extends AnyFlatSpec with Matchers {
  it should "modify an object declared using type alias" in {
    val s: S = State(0)
    val modified = s.modify(_.x).setTo(1)

    modified.x shouldBe 1
  }
}
