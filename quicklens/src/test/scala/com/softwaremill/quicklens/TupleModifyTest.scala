package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TupleModifyTest extends AnyFlatSpec with Matchers {
  it should "modify tuples using setTo" in {
    val tuple4 = (0, 1, 2, 3)
    val modified = tuple4.modify(_._3).setTo(3)
    modified shouldBe(0, 1, 3, 3)
  }

  it should "modify tuples using using" in {
    val tuple4 = (0, 1, 2, 3)
    val modified = tuple4.modify(_._3).using(_ + 1)
    modified shouldBe(0, 1, 3, 3)
  }

  it should "modify tuples using multiple modify" in {
    val tuple4 = (0, 1, 2, 3)
    val modified = tuple4.modify(_._3).using(_ + 1).modify(_._4).using(_ + 1)
    modified shouldBe (0, 1, 3, 4)
  }
}
