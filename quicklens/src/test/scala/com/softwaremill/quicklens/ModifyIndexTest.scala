package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifyIndexTest extends AnyFlatSpec with Matchers {

  it should "modify a non-nested list with case class item" in {
    modify(l1)(_.index(2).a4.a5.name).using(duplicate) should be(l1at2dup)
    modify(l1)(_.index(2))
      .using(a3 => modify(a3)(_.a4.a5.name).using(duplicate)) should be(l1at2dup)
  }

  it should "modify a nested list using index" in {
    modify(ll1)(_.index(2).index(1).name).using(duplicate) should be(ll1at2at1dup)
  }

  it should "modify a nested list using index and each" in {
    modify(ll1)(_.index(2).each.name).using(duplicate) should be(ll1at2eachdup)
    modify(ll1)(_.each.index(1).name).using(duplicate) should be(ll1eachat1dup)
  }

  it should "modify both lists and options" in {
    modify(y1)(_.y2.y3.index(1).y4.each.name).using(duplicate) should be(y1at1dup)
  }

  it should "not modify if given index does not exist" in {
    modify(l1)(_.index(10).a4.a5.name).using(duplicate) should be(l1)
  }
}
