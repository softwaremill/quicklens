package com.softwaremill.quicklens

import java.util.NoSuchElementException

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifyOptionAtTest extends AnyFlatSpec with Matchers {

  it should "modify a Option with case class item" in {
    modify(Option(1))(_.at).using(_ + 1) should be(Option(2))
  }

  it should "modify a Option in a case class hierarchy" in {
    case class Foo(a: Int)
    case class Bar(foo: Foo)
    case class BarOpt(maybeBar: Option[Bar])
    case class BazOpt(barOpt: BarOpt)
    modify(BazOpt(BarOpt(Some(Bar(Foo(4))))))(_.barOpt.maybeBar.at.foo.a).using(_ + 1) should be(
      BazOpt(BarOpt(Some(Bar(Foo(5)))))
    )
  }

  it should "crashes on missing key" in {
    an[NoSuchElementException] should be thrownBy modify(None: Option[Int])(_.at).using(_ + 1)
  }
}
