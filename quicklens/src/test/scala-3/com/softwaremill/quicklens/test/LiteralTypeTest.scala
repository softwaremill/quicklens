package com.softwaremill.quicklens.test

import com.softwaremill.quicklens.TestData.duplicate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.softwaremill.quicklens._

object LiteralTypeTestData {
  case class Test(f: "foo")
  case class Test1[A](f: A)
}

class LiteralTypeTest extends AnyFlatSpec with Matchers {
  import LiteralTypeTestData._

  it should "modify a literal type field with an explicit parameter" in {
    Test("foo").modify["foo"](_.f).setTo("foo") should be(Test("foo"))
  }

  it should "modify a literal type field as a type parameter with an explicit parameter" in {
    Test1["foo"]("foo").modify["foo"](_.f).setTo("foo") should be(Test1("foo"))
  }

  it should "not compile for a wrong literal type" in {
    assertDoesNotCompile("""
      import com.softwaremill.quicklens.*
      
      case class Test1[A](f: A)
      
      Test1["foo"]("foo").modify["foo"](_.f).setTo("bar")
    """)
  }
}