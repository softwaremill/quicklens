package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.{FlatSpec, Matchers}

class SetToSimpleTest extends FlatSpec with Matchers {
  it should "set a new value of a single-nested case class field" in {
    modify(a1)(_.a2.a3.a4.a5.name).setTo("mod") should be (a1mod)
  }
}
