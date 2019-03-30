package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.{FlatSpec, Matchers}

class ModifyLazyTest extends FlatSpec with Matchers {
  it should "modify a single-nested case class field" in {
    val ml = modify[A5](_.name).using(duplicate)
    ml(a5) should be(a5dup)
  }

  it should "modify a deeply-nested case class field" in {
    val ml = modify[A1](_.a2.a3.a4.a5.name).using(duplicate)
    ml(a1) should be(a1dup)
  }

  it should "modify several fields" in {
    val ml = modifyAll[B1](_.b2, _.b3.each).using(duplicate)
    ml(b1) should be(b1dupdup)
  }

  it should "modify a case class field if the condition is true" in {
    val ml = modify[A5](_.name).usingIf(true)(duplicate)
    ml(a5) should be(a5dup)
  }

  it should "leave a case class unchanged if the condition is flase" in {
    val ml = modify[A5](_.name).usingIf(false)(duplicate)
    ml(a5) should be(a5)
  }
}
