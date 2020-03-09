package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifyGetOrElseTest extends AnyFlatSpec with Matchers {

  it should "modify a Some" in {
    modify(Option(1))(_.getOrElse(3)).using(_ + 1) should be(Option(2))
  }

  it should "modify a None with default" in {
    modify(None: Option[Int])(_.getOrElse(3)).using(_ + 1) should be(Option(4))
  }

  it should "modify a Option in a case class hierarchy" in {
    case class Foo(a: Int)
    case class Bar(foo: Foo)
    case class BarOpt(maybeBar: Option[Bar])
    case class BazOpt(barOpt: BarOpt)
    modify(BazOpt(BarOpt(Some(Bar(Foo(4))))))(_.barOpt.maybeBar.get.foo.a).using(_ + 1) should be(
      BazOpt(BarOpt(Some(Bar(Foo(5)))))
    )
  }
}
