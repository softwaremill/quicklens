package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifyPimpTest extends AnyFlatSpec with Matchers {
  it should "modify a field once" in {
    a1.modify(_.a2.a3.a4.a5.name).using(duplicate) should be(a1dup)
  }

  it should "modify a deeply-nested case class field" in {
    a1.modify(_.a2.a3.a4.a5.name)
      .using(duplicate)
      .modify(_.a2.a3.a4.a5.name)
      .using(duplicate) should be(a1dupdup)
  }

  it should "modify several fields" in {
    b1.modifyAll(_.b2, _.b3.each).using(duplicate) should be(b1dupdup)
  }

  it should "modify polymorphic case class field" in {
    aPoly.modify(_.poly).using(duplicate) should be(aPolyDup)
  }
}
