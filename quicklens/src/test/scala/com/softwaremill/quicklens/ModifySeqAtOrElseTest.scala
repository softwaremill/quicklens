package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifySeqAtOrElseTest extends AnyFlatSpec with Matchers {

  it should "modify an existing element of a list using atOrElse" in {
    modify(l1)(_.atOrElse(2, A3(A4(A5("d4")))).a4.a5.name).using(duplicate) should be(l1at2dup)
  }

  it should "not throw for a missing index, using the default instead" in {
    val items = List(A5("a"), A5("b"))
    noException should be thrownBy {
      modify(items)(_.atOrElse(5, A5("default")).name).using(duplicate)
    }
  }
}
