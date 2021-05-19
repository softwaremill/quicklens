package com.softwaremill.quicklens.test

import com.softwaremill.quicklens.TestData.duplicate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.softwaremill.quicklens._

object EnumTestData {
  enum P3(val a: String):
    case C5(override val a: String, b: Int) extends P3(a)
    case C6(override val a: String, c: Option[String]) extends P3(a)

  val p3: P3 = P3.C5("c2", 0)
  val p3dup: P3 = P3.C5("c2c2", 0)
}

class ModifyEnumTest extends AnyFlatSpec with Matchers {
  import EnumTestData._

  it should "modify a field in an enum case" in {
    modify(p3)(_.a).using(duplicate) should be(p3dup)
  }

  it should "modify a field in an enum case with extension method" in {
    p3.modify(_.a).using(duplicate) should be(p3dup)
  }
}
