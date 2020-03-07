package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SetToSimpleTest extends AnyFlatSpec with Matchers {
  it should "set a new value of a single-nested case class field" in {
    modify(a1)(_.a2.a3.a4.a5.name).setTo("mod") should be(a1mod)
  }

  it should "set a new value in a case class if the condition is true" in {
    modify(a1)(_.a2.a3.a4.a5.name).setToIf(true)("mod") should be(a1mod)
  }

  it should "leave a case class unchanged if the condition is false" in {
    modify(a1)(_.a2.a3.a4.a5.name).setToIf(false)("mod") should be(a1)
  }

  it should "set a new value in a case class if it is defined" in {
    modify(a1)(_.a2.a3.a4.a5.name).setToIfDefined(Some("mod")) should be(a1mod)
  }

  it should "leave a case class unchanged if the value is not defined" in {
    modify(a1)(_.a2.a3.a4.a5.name).setToIfDefined(None) should be(a1)
  }
}
