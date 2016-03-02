package com.softwaremill.quicklens

import org.scalatest.{FlatSpec, Matchers}
import TestData._

class ModifySimpleTest extends FlatSpec with Matchers {
  it should "modify a single-nested case class field" in {
    modify(a5)(_.name).using(duplicate) should be (a5dup)
  }

  it should "modify a deeply-nested case class field" in {
    modify(a1)(_.a2.a3.a4.a5.name).using(duplicate) should be (a1dup)
  }

  it should "modify several fields" in {
    modifyAll(b1)(_.b2, _.b3.each).using(duplicate) should be (b1dupdup)
  }
}
