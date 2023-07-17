package com.softwaremill.quicklens.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.softwaremill.quicklens.*

class CustomModifyProxyTest extends AnyFlatSpec with Matchers {

  it should "correctly modify a class using a custom modify proxy method" in {
    case class State(foo: Int)

    inline def set[A](state: State, inline path: State => A, value: A): State = {
      modify(state)(path).setTo(value)
    }

    val state = State(100)
    val res = set(state, _.foo, 200)
    val expected = State(200)
    res.shouldBe(expected)
  }

}
