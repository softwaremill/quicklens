package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ModifyAliasTest._

object ModifyAliasTest {

  case class State(x: Int)

  type S = State

  sealed trait Expr {
    def i: Int
  }
  case class ListInt(i: Int) extends Expr

  type E = Expr
}

class ModifyAliasTest extends AnyFlatSpec with Matchers {
  it should "modify an object declared using type alias" in {
    val s: S = State(0)
    val modified = s.modify(_.x).setTo(1)

    modified.x shouldBe 1
  }

  it should "modify a sealed hierarchy declared using type alias" in {
    val s: E = ListInt(0)
    val modified = s.modify(_.i).setTo(1)

    modified.i shouldBe 1
  }
}
