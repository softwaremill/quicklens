package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifySeqAtOrElseTest extends AnyFlatSpec with Matchers {

  it should "modify a seq with atOrElse when the index exists" in {
    modify(s1)(_.atOrElse(2, A3(A4(A5("d4")))).a4.a5.name).using(duplicate) should be(l1at2dup)
  }

  it should "not throw for a missing seq index when using atOrElse" in {
    noException should be thrownBy {
      modify(s1)(_.atOrElse(10, A3(A4(A5("d4")))).a4.a5.name).using(duplicate)
    }
  }

  it should "modify an array with atOrElse when the index exists" in {
    modify(ar1)(_.atOrElse(2, A3(A4(A5("d4")))).a4.a5.name).using(duplicate).toList should be(l1at2dup)
  }

  it should "not throw for a missing array index when using atOrElse" in {
    noException should be thrownBy {
      modify(ar1)(_.atOrElse(10, A3(A4(A5("d4")))).a4.a5.name).using(duplicate)
    }
  }
}
