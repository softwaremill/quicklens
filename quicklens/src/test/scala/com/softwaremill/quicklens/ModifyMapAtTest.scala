package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifyMapAtTest extends AnyFlatSpec with Matchers {

  it should "modify a non-nested map with case class item" in {
    modify(m1)(_.at("K1").a5.name).using(duplicate) should be(m1dup)
  }

  it should "modify a non-nested map with atOrElse" in {
    modify(m1)(_.atOrElse("K1", A4(A5("d4"))).a5.name).using(duplicate) should be(m1dup)
    modify(m1)(_.atOrElse("K1", ???).a5.name).using(duplicate) should be(m1dup)
    modify(m1)(_.atOrElse("K4", A4(A5("d4"))).a5.name).using(duplicate) should be(m1missingdup)
  }

  it should "modify a non-nested sorted map with case class item" in {
    modify(ms1)(_.at("K1").a5.name).using(duplicate) should be(m1dup)
  }

  it should "modify a non-nested hash map with case class item" in {
    modify(mh1)(_.at("K1").a5.name).using(duplicate) should be(m1dup)
  }

  it should "modify a non-nested listed map with case class item" in {
    modify(ml1)(_.at("K1").a5.name).using(duplicate) should be(m1dup)
  }

  it should "modify a nested map using at" in {
    modify(m2)(_.m3.at("K1").a5.name).using(duplicate) should be(m2dup)
  }

  it should "modify a nested map using atOrElse" in {
    modify(m2)(_.m3.atOrElse("K4", A4(A5("d4"))).a5.name).using(duplicate) should be(m2missingdup)
  }

  it should "modify a non-nested map using each" in {
    modify(m1)(_.each.a5.name).using(duplicate) should be(m1dupEach)
  }

  it should "modify a non-nested sorted map using each" in {
    modify(ms1)(_.each.a5.name).using(duplicate) should be(m1dupEach)
  }

  it should "modify a non-nested hash map using each" in {
    modify(mh1)(_.each.a5.name).using(duplicate) should be(m1dupEach)
  }

  it should "modify a non-nested list map using each" in {
    modify(ml1)(_.each.a5.name).using(duplicate) should be(m1dupEach)
  }

  it should "throw an exception if there's no such element" in {
    an[NoSuchElementException] should be thrownBy {
      modify(m1)(_.at("K0").a5.name).using(duplicate)
    }
  }

  it should "modify a map using at with a derived class" in {
    class C
    object D extends C
    val m = Map[C, String](D -> "")
    val expected = Map(D -> "x")
    modify(m)(_.at(D)).setTo("x") should be(expected)
  }
}
