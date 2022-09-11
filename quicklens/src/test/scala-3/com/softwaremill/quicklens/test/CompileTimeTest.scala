package com.softwaremill.quicklens.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompileTimeTest extends AnyFlatSpec with Matchers {
  // #114
  it should "not compile for too long in case of chained modify invocations" in {
    val start = System.currentTimeMillis()
    assertDoesNotCompile("""
      case class B(a1: Double, a2: Double, a3: Double, a4: Double, a5: Double)
      case class C(b: B)
      
      import com.softwaremill.quicklens.*
      
      val c = C(B(1, 1, 1, 1, 1))
      c
        .modify(_.b.a1).setTo("")
        .modify(_.b.a2).setTo("")
        .modify(_.b.a3).setTo("")
        .modify(_.b.a4).setTo("")
        .modify(_.b.a5).setTo("")
  """)
    val end = System.currentTimeMillis()
    (end - start) shouldBe <=(5000L) // that's a lot anyway
  }
}
