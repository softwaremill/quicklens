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

  it should "modify an existing element of an Array using atOrElse" in {
    val items = Array(Item("a"), Item("b"))
    modify(items)(_.atOrElse(1, Item("default")).name).using(_.toUpperCase).toList should be(
      List(Item("a"), Item("B"))
    )
  }

  it should "append the modified default for an index outside the Seq" in {
    val items = List(Item("a"), Item("b"))
    modify(items)(_.atOrElse(5, Item("default")).name).using(_.toUpperCase) should be(
      List(Item("a"), Item("b"), Item("DEFAULT"))
    )
  }

  it should "append the modified default for an index outside an Array" in {
    val items = Array(Item("a"), Item("b"))
    modify(items)(_.atOrElse(5, Item("default")).name).using(_.toUpperCase).toList should be(
      List(Item("a"), Item("b"), Item("DEFAULT"))
    )
  }
}
