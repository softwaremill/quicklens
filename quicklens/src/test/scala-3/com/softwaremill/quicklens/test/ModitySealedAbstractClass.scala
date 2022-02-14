package com.softwaremill.quicklens.test

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.softwaremill.quicklens._

class ModitySealedAbstractClass extends AnyFlatSpec with Matchers {
  it should "Modify abstract class hierarchy" in {
    invInt.modify(_.typ).setTo(Type("Long")) should be(invLong)
  }
}
