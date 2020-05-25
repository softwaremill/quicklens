package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifyOptionIndexTest extends AnyFlatSpec with Matchers {

  it should "modify a Option with case class item" in {
    modify(Option(1))(_.index).using(_ + 1) should be(Option(2))
  }

  it should "modify a Option in a case class hierarchy" in {
    case class Foo(a: Int)
    case class Bar(foo: Foo)
    case class BarOpt(maybeBar: Option[Bar])
    case class BazOpt(barOpt: BarOpt)
    modify(BazOpt(BarOpt(Some(Bar(Foo(4))))))(_.barOpt.maybeBar.index.foo.a).using(_ + 1) should be(
      BazOpt(BarOpt(Some(Bar(Foo(5)))))
    )
  }

  it should "not modify on missing key" in {
    modify(Option.empty[Int])(_.index).using(_ + 1) should be(None)
  }
}
