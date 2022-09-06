package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EnormousModifyAllTest extends AnyFlatSpec with Matchers {
  import EnormousModifyAllTest._

  it should "expand an enormous function" in {
    val c6 = C6(1)
    val c5 = C5(c6, c6, c6, c6)
    val c4 = C4(c5, c5, c5, c5)
    val c3 = C3(c4, c4, c4, c4)
    val c2 = C2(c3, c3, c3, c3)
    val c1 = C1(c2, c2, c2, c2)

    val c6e = C6(2)
    val c5e = C5(c6e, c6, c6, c6)
    val c4e = C4(c5e, c5, c5, c5)
    val c3e = C3(c4e, c4, c4, c4)
    val c2e = C2(c3e, c3, c3, c3)
    val c1e = C1(c2e, c2e, c2e, c2e)

    val res = c1
      .modifyAll(
        _.a.a.a.a.a.a,
        _.b.a.a.a.a.a,
        _.c.a.a.a.a.a,
        _.d.a.a.a.a.a
      )
      .using(_ + 1)
    res should be(c1e)
  }
}

object EnormousModifyAllTest {
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
      a: C6,
      b: C6,
      c: C6,
      d: C6
  )

  case class C6(
      a: Int
  )
}
