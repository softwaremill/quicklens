package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData.duplicate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** This test data is in the same file as the test to ensure correct compilation order. See
  * https://issues.scala-lang.org/browse/SI-7046.
  */
object SealedTestData {
  case class G(p1: Option[P1])
  sealed trait P1 {
    def x: String
    def f: Option[String]
  }
  case class C1(x: String, f: Option[String]) extends P1
  case class C2(x: String, f: Option[String]) extends P1

  val p1: P1 = C2("c2", None)
  val p1dup: P1 = C2("c2c2", None)

  val g1 = G(Some(C1("c1", Some("c2"))))
  val g1dup = G(Some(C1("c1c1", Some("c2"))))
  val g1eachdup = G(Some(C1("c1", Some("c2c2"))))

  sealed trait P2 {
    def x: String
  }
  case class C3(x: String) extends P2
  sealed trait P3 extends P2
  case class C4(x: String) extends P3

  val p2: P2 = C4("c4")
  val p2dup: P2 = C4("c4c4")

  // example from the README

  sealed trait Pet { def name: String }
  case class Fish(name: String) extends Pet
  sealed trait LeggedPet extends Pet
  case class Cat(name: String) extends LeggedPet
  case class Dog(name: String) extends LeggedPet

  val pets = List[Pet](Fish("Finn"), Cat("Catia"), Dog("Douglas"))
  val juniorPets =
    List[Pet](Fish("Finn, Jr."), Cat("Catia, Jr."), Dog("Douglas, Jr."))
}

class SealedTest extends AnyFlatSpec with Matchers {
  import SealedTestData._

  it should "modify a field in a sealed trait" in {
    modify(p1)(_.x).using(duplicate) should be(p1dup)
  }

  it should "modify a field in a sealed trait through a Functor" in {
    modify(g1)(_.p1.each.x).using(duplicate) should be(g1dup)
  }

  it should "modify a Functor field in a sealed trait" in {
    modify(g1)(_.p1.each.f.each).using(duplicate) should be(g1eachdup)
  }

  it should "modify a field in a hierarchy of sealed traits" in {
    modify(p2)(_.x).using(duplicate) should be(p2dup)
  }

  it should "modify a list of pets from the example" in {
    modify(pets)(_.each.name).using(_ + ", Jr.") should be(juniorPets)
  }
}
