package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModifySeqAtOrElseTest extends AnyFlatSpec with Matchers {

  case class Item(name: String)

  it should "modify an existing element using atOrElse" in {
    val items = List(Item("a"), Item("b"))
    modify(items)(_.atOrElse(1, Item("default")).name).using(_.toUpperCase) should be(
      List(Item("a"), Item("B"))
    )
  }

  it should "use the default for a missing index instead of throwing" in {
    val items = List(Item("a"), Item("b"))
    modify(items)(_.atOrElse(5, Item("default")).name).using(_.toUpperCase) should be(
      List(Item("a"), Item("b"), Item("DEFAULT"))
    )
  }
}
