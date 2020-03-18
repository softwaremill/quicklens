package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import TestData._

class ModifySimpleTest extends AnyFlatSpec with Matchers {
  it should "modify a single-nested case class field" in {
    modify(a5)(_.name).using(duplicate) should be(a5dup)
  }

  it should "modify a single-nested case class field using apply" in {
    modify(a5)(_.name)(duplicate) should be(a5dup)
  }

  it should "modify a deeply-nested case class field" in {
    modify(a1)(_.a2.a3.a4.a5.name).using(duplicate) should be(a1dup)
  }

  it should "modify several fields" in {
    modifyAll(b1)(_.b2, _.b3.each).using(duplicate) should be(b1dupdup)
  }

  it should "modify a case class field if the condition is true" in {
    modify(a5)(_.name).usingIf(true)(duplicate) should be(a5dup)
  }

  it should "leave a case class unchanged if the condition is flase" in {
    modify(a5)(_.name).usingIf(false)(duplicate) should be(a5)
  }
}
