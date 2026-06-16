package com.softwaremill.quicklens

import com.softwaremill.quicklens.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifyArrayAtOrElseTest extends AnyFlatSpec with Matchers {

  it should "modify an existing element using atOrElse" in {
    modify(ar1)(_.atOrElse(2, A3(A4(A5("default")))).a4.a5.name).using(duplicate) should be(l1at2dup)
  }

  it should "append the modified default for a missing index" in {
    modify(ar1)(_.atOrElse(10, A3(A4(A5("def")))).a4.a5.name).using(duplicate) should be(
      l1 :+ A3(A4(A5("defdef")))
    )
  }
}
