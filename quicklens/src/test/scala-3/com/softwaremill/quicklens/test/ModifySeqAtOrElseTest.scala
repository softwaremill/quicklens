package com.softwaremill.quicklens
package test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifySeqAtOrElseTest extends AnyFlatSpec with Matchers {

  case class Item(name: String)

  it should "modify an existing element of a Seq using atOrElse" in {
    val items = List(Item("a"), Item("b"))
    modify(items)(_.atOrElse(1, Item("default")).name).using(_.toUpperCase) should be(
      List(Item("a"), Item("B"))
    )
  }

  it should "not throw for an index outside the Seq, using the default" in {
    val items = List(Item("a"), Item("b"))
    noException should be thrownBy {
      modify(items)(_.atOrElse(5, Item("default")).name).using(_.toUpperCase)
    }
  }

  it should "not throw for an index outside an Array, using the default" in {
    val items = Array(Item("a"), Item("b"))
    noException should be thrownBy {
      modify(items)(_.atOrElse(5, Item("default")).name).using(_.toUpperCase)
    }
  }
}
