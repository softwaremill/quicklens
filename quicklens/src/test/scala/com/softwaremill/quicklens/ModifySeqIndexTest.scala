package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifySeqIndexTest extends AnyFlatSpec with Matchers {

  it should "modify a non-nested seq with case class item" in {
    modify(s1)(_.index(2).a4.a5.name).using(duplicate) should be(l1at2dup)
    modify(s1)(_.index(2))
      .using(a3 => modify(a3)(_.a4.a5.name).using(duplicate)) should be(l1at2dup)
  }

  it should "modify a nested seq using index" in {
    modify(ss1)(_.index(2).index(1).name).using(duplicate) should be(ll1at2at1dup)
  }

  it should "modify a nested seq using index and each" in {
    modify(ss1)(_.index(2).each.name).using(duplicate) should be(ll1at2eachdup)
    modify(ss1)(_.each.index(1).name).using(duplicate) should be(ll1eachat1dup)
  }

  it should "not modify if given index does not exist" in {
    modify(s1)(_.index(10).a4.a5.name).using(duplicate) should be(l1)
  }
}
