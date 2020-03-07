package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

case class Named(name: String)

case class Aged(age: Int)

case class Eithers(e: Either[Named, Aged])

case class MoreEithers(e1: Either[Eithers, MoreEithers], e2: Either[Eithers, MoreEithers])

class ModifyEitherTest extends AnyFlatSpec with Matchers {

  it should "modify a single-nested left case class field" in {
    modify(
      Eithers(Left(Named("boo")))
    )(_.e.eachLeft.name).setTo("moo") should be(
      Eithers(Left(Named("moo")))
    )
  }

  it should "modify a single-nested left case class field (pimped)" in {
    Eithers(Left(Named("boo")))
      .modify(_.e.eachLeft.name)
      .setTo("moo") should be(
      Eithers(Left(Named("moo")))
    )
  }

  it should "modify a single-nested right case class field" in {
    modify(
      Eithers(Right(Aged(23)))
    )(_.e.eachRight.age).setTo(32) should be(
      Eithers(Right(Aged(32)))
    )
  }

  it should "modify a single-nested right case class field (pimped)" in {
    Eithers(Right(Aged(23)))
      .modify(_.e.eachRight.age)
      .setTo(32) should be(
      Eithers(Right(Aged(32)))
    )
  }

  it should "modify multiple deeply-nested either case class fields" in {

    modify(
      MoreEithers(
        e1 = Right(
          MoreEithers(
            e1 = Left(Eithers(Right(Aged(23)))),
            e2 = Left(Eithers(Left(Named("boo"))))
          )
        ),
        e2 = Left(Eithers(Left(Named("boo"))))
      )
    )(_.e1.eachRight.e2.eachLeft.e.eachLeft.name)
      .using(_.toUpperCase) should be(
      MoreEithers(
        e1 = Right(
          MoreEithers(
            e1 = Left(Eithers(Right(Aged(23)))),
            e2 = Left(Eithers(Left(Named("BOO"))))
          )
        ),
        e2 = Left(Eithers(Left(Named("boo"))))
      )
    )
  }

  it should "not modify left case class field if it is right" in {
    modify(
      Eithers(Right(Aged(23)))
    )(_.e.eachLeft.name).setTo("moo") should be(
      Eithers(Right(Aged(23)))
    )
  }

  it should "not modify right case class field if it is left" in {
    modify(
      Eithers(Left(Named("boo")))
    )(_.e.eachRight.age).setTo(33) should be(
      Eithers(Left(Named("boo")))
    )
  }

  it should "allow .eachLeft at then end" in {
    modify(Left("boo"): Either[String, Int])(_.eachLeft)
      .using(_.toUpperCase) should be(Left("BOO"))
  }

  it should "allow .eachRight at then end" in {
    modify(Right(23): Either[String, Int])(_.eachRight).using(_ + 3) should be(Right(26))
  }
}
