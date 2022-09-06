package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HugeModifyTest extends AnyFlatSpec with Matchers {
  import HugeModifyTestData._

  it should "expand a huge function" in {
    val c5 = C5(1)
    val c4 = C4(c5, c5, c5, c5)
    val c3 = C3(c4, c4, c4, c4)
    val c2 = C2(c3, c3, c3, c3)
    val c1 = C1(c2, c2, c2, c2)

    val c5e = C5(2)
    val c4e = C4(c5e, c5, c5, c5)
    val c3e = C3(c4e, c4, c4, c4)
    val c2e = C2(c3e, c3, c3, c3)
    val c1e = C1(c2e, c2e, c2e, c2)

    val res = c1
      .modifyAll(
        _.a.a.a.a.a,
        _.b.a.a.a.a,
        _.c.a.a.a.a
      )
      .using(_ + 1)
    res should be(c1e)
  }
}

object HugeModifyTestData {
  case class C1(
      a: C2,
      b: C2,
      c: C2,
      d: C2
  )

  case class C2(
      a: C3,
      b: C3,
      c: C3,
      d: C3
  )

  case class C3(
      a: C4,
      b: C4,
      c: C4,
      d: C4
  )

  case class C4(
      a: C5,
      b: C5,
      c: C5,
      d: C5
  )

  case class C5(
      a: Int
  )
}
