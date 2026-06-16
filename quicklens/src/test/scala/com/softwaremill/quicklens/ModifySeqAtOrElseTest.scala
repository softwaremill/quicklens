package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifySeqAtOrElseTest extends AnyFlatSpec with Matchers {

  it should "modify an existing index of a seq using atOrElse" in {
    modify(s1)(_.atOrElse(2, A3(A4(A5("d3")))).a4.a5.name).using(duplicate) should be(l1at2dup)
  }

  it should "use the default when the index is out of bounds instead of throwing" in {
    modify(s1)(_.atOrElse(10, A3(A4(A5("d5")))).a4.a5.name).using(duplicate) should be(
      l1 :+ A3(A4(A5("d5d5")))
    )
  }
}
