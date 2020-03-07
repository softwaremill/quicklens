package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifyEachTest extends AnyFlatSpec with Matchers {
  it should "modify a single-nested optional case class field" in {
    modify(x4)(_.x5.each.name).using(duplicate) should be(x4dup)
  }

  it should "modify a single-nested optional case class field (pimped)" in {
    x4.modify(_.x5.each.name).using(duplicate) should be(x4dup)
  }

  it should "modify multiple deeply-nested optional case class field" in {
    modify(x1)(_.x2.x3.each.x4.x5.each.name).using(duplicate) should be(x1dup)
  }

  it should "not modify an optional case class field if it is none" in {
    modify(x1none)(_.x2.x3.each.x4.x5.each.name).using(duplicate) should be(x1none)
    modify(x4none)(_.x5.each.name).using(duplicate) should be(x4none)
  }

  it should "modify both lists and options" in {
    modify(y1)(_.y2.y3.each.y4.each.name).using(duplicate) should be(y1dup)
  }

  it should "allow .each at the end" in {
    modify(z1)(_.name.each).using(duplicate) should be(z1dup)
  }
}
