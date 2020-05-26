package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifyMapIndexTest extends AnyFlatSpec with Matchers {

  it should "modify a non-nested map with case class item" in {
    modify(m1)(_.index("K1").a5.name).using(duplicate) should be(m1dup)
  }

  it should "modify a non-nested sorted map with case class item" in {
    modify(ms1)(_.index("K1").a5.name).using(duplicate) should be(m1dup)
  }

  it should "modify a non-nested hash map with case class item" in {
    modify(mh1)(_.index("K1").a5.name).using(duplicate) should be(m1dup)
  }

  it should "modify a non-nested listed map with case class item" in {
    modify(ml1)(_.index("K1").a5.name).using(duplicate) should be(m1dup)
  }

  it should "modify a nested map using index" in {
    modify(m2)(_.m3.index("K1").a5.name).using(duplicate) should be(m2dup)
  }

  it should "not modify if there's no such element" in {
    modify(m1)(_.index("K0").a5.name).using(duplicate) should be(m1)
  }
}
