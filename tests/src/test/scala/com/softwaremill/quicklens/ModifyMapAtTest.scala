package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.{ FlatSpec, ShouldMatchers }

class ModifyMapAtTest extends FlatSpec with ShouldMatchers {

  it should "modify a non-nested map with case class item" in {
    modify(m1)(_.at("K1").a5.name).using(duplicate) should be(m1dup)
    //modify(l1)(_.at(2)).using(a3 => modify(a3)(_.a4.a5.name).using(duplicate)) should be(l1at2dup)
  }

  it should "modify a nested map using at" in {
    modify(m2)(_.m3.at("K1").a5.name).using(duplicate) should be(m2dup)
  }

  it should "throw an exception if there's no such element" in {
    an[NoSuchElementException] should be thrownBy { modify(m1)(_.at("K0").a5.name).using(duplicate) }
  }
}
