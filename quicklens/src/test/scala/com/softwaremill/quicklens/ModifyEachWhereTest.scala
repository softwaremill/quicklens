package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifyEachWhereTest extends AnyFlatSpec with Matchers {
  it should "modify a single-nested optional case class field only if the condition returns true" in {
    modify(x4)(_.x5.eachWhere(_ => true).name).using(duplicate) should be(x4dup)
    modify(x4)(_.x5.eachWhere(_ => false).name).using(duplicate) should be(x4)
  }

  it should "modify a single-nested optional case class field (pimped) only if the condition returns true" in {
    x4.modify(_.x5.eachWhere(_ => true).name).using(duplicate) should be(x4dup)
    x4.modify(_.x5.eachWhere(_ => false).name).using(duplicate) should be(x4)
  }

  it should "modify be able to eachWhere a lambda" in {
    val foo = true
    x4.modify(_.x5.eachWhere(_ => foo).name).using(duplicate) should be(x4dup)
    val bar = false
    x1.modify(_.x2.x3.eachWhere(_ => bar).x4.x5.eachWhere(_ => bar).name).using(duplicate) should be(x1)
  }

  it should "modify be able to eachWhere a lambda 1" in {
    val one = "1"
    person
      .modify(_.addresses.each.street.eachWhere(_.name.startsWith(one)).name)
      .using(_.toUpperCase)
  }

  it should "allow referencing local variable" in {
    val zmienna = "d"
    modify(z1)(_.name.eachWhere(_.startsWith(zmienna))).using(duplicate) should be(z1dup)
  }

  it should "allow referencing local variable 1" in {
    val lang2 = "hey"

    Seq(Foo("asdf"))
      .modify(_.eachWhere(_.field != lang2).field)
      .setTo(lang2) should be (Seq(Foo("hey")))
  }

  it should "not modify an optional case class field if it is none regardless of the condition" in {
    modify(x4none)(_.x5.eachWhere(_ => true).name).using(duplicate) should be(x4none)
    modify(x4none)(_.x5.eachWhere(_ => false).name).using(duplicate) should be(x4none)
  }

  it should "modify only those list elements where the condition returns true" in {
    modify(y1)(_.y2.y3.eachWhere(_.y4.map(_.name) == Some("d2")).y4.each.name)
      .using(duplicate) should be(y1at1dup)
  }

  it should "allow .each at then end only if the condition returns true" in {
    modify(z1)(_.name.eachWhere(_.startsWith("d"))).using(duplicate) should be(z1dup)
    modify(z1)(_.name.eachWhere(_.startsWith("e"))).using(duplicate) should be(z1)
  }
}
