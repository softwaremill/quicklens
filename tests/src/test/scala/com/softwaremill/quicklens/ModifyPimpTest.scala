package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.{FlatSpec, ShouldMatchers}

class ModifyPimpTest extends FlatSpec with ShouldMatchers {
  it should "modify a field once" in {
    a1.modify(_.a2.a3.a4.a5.name).using(duplicate) should be (a1dup)
  }

  it should "modify a deeply-nested case class field" in {
    a1.modify(_.a2.a3.a4.a5.name).using(duplicate)
      .modify(_.a2.a3.a4.a5.name).using(duplicate) should be (a1dupdup)
  }
}
