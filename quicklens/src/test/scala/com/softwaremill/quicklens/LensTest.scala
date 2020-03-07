package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LensTest extends AnyFlatSpec with Matchers {
  it should "create reusable lens of the given type" in {
    val lens = modify(_: A1)(_.a2.a3.a4.a5.name)

    lens(a1).using(duplicate) should be(a1dup)
  }

  it should "compose lens" in {
    val lens_a1_a3 = modify(_: A1)(_.a2.a3)
    val lens_a3_name = modify(_: A3)(_.a4.a5.name)

    (lens_a1_a3 andThenModify lens_a3_name)(a1).using(duplicate) should be(a1dup)
  }
}
