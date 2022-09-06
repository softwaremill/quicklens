package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import scala.reflect.ClassTag

class ModifyAllOptimizedTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {
  import ModifyAllOptimizedTest._

  override def beforeEach() = {
    Cons.copyCount = 0
    ConsOpt.copyCount = 0
    Opt.eachCount = 0
  }

  it should "Have a correct number of copy calls with single focus" in {
    val lst: Cons = Cons.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.head
      )
      .using(_ + 1)

    Cons.copyCount should be(1)
  }

  it should "optimize number of copy calls 1" in {
    val lst: Cons = Cons.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.head,
        _.tail.each.head
      )
      .using(_ + 1)

    Cons.copyCount should be(2)
  }

  it should "optimize number of copy calls 2" in {
    val lst: Cons = Cons.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.head,
        _.tail.each.head,
        _.tail.each.tail.each.head
      )
      .using(_ + 1)

    Cons.copyCount should be(3)
  }

  it should "optimize number of copy calls 3" in {
    val lst: Cons = Cons.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.tail.each.tail.each.head,
        _.tail.each.tail.each.tail.each.head
      )
      .using(_ + 1)

    Cons.copyCount should be(4)
  }

  it should "optimize number of copy calls 4" in {
    val lst: Cons = Cons.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.tail,
        _.tail.each.tail,
        _.tail,
        _.tail.each.tail
      )
      .using {
        case None                   => None
        case Some(Cons(head, tail)) => Some(Cons(head + 1, tail))
      }

    Cons.copyCount should be(3)
  }

  it should "optimize number of copy calls 5" in {
    val lst: Cons = Cons.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.tail,
        _.tail.each.tail,
        _.tail.each.tail,
        _.tail
      )
      .using {
        case None                   => None
        case Some(Cons(head, tail)) => Some(Cons(head + 1, tail))
      }

    Cons.copyCount should be(2)
  }

  it should "optimize number of copy calls 6" in {
    val lst: Cons = Cons.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.tail,
        _.tail.each.tail.each.tail,
        _.tail.each.tail,
        _.tail.each.tail.each.tail.each.tail,
        _.tail.each.tail,
        _.tail
      )
      .using {
        case None                   => None
        case Some(Cons(head, tail)) => Some(Cons(head + 1, tail))
      }

    Cons.copyCount should be(5)
  }

  it should "optimize number of each function delegates 1" in {
    val lst: ConsOpt = ConsOpt.fromList(List(1, 2, 3)).get

    lst
      .modifyAll(
        _.tail.each.head,
        _.tail.each.tail.each.head
      )
      .using(_ + 1)

    Opt.eachCount should be(2)
  }

  it should "optimize number of each function delegates 2" in {
    val lst: ConsOpt = ConsOpt.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.tail.each.head,
        _.tail.each.tail.each.head
      )
      .using(_ + 1)

    Opt.eachCount should be(2)
  }

  it should "optimize number of each function delegates 3" in {
    val lst: ConsOpt = ConsOpt.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.tail.each.tail.each.head,
        _.tail.each.tail.each.tail.each.head
      )
      .using(_ + 1)

    Opt.eachCount should be(3)
  }
}

object ModifyAllOptimizedTest {

  case class Cons(head: Int, tail: Option[Cons]) {
    def copy(head: Int = head, tail: Option[Cons] = tail): Cons = {
      Cons.copyCount = Cons.copyCount + 1
      Cons(head, tail)
    }
  }
  object Cons {
    var copyCount = 0
    def fromList(list: List[Int]): Option[Cons] = list match {
      case Nil => None
      case head :: tail =>
        Some(Cons(head, fromList(tail)))
    }
  }

  sealed trait Opt[+A] {
    def get: A
  }
  object Opt {
    var eachCount = 0
  }
  case object Nada extends Opt[Nothing] {
    override def get: Nothing = ???
  }
  case class Just[+A](val a: A) extends Opt[A] {
    override def get: A = a
    override def equals(other: Any): Boolean = other match {
      case Just(a1) => a == a1
      case _        => false
    }
  }

  given QuicklensFunctor[Opt] with {
    def map[A](fa: Opt[A], f: A => A): Opt[A] =
      Opt.eachCount = Opt.eachCount + 1
      fa match {
        case Nada    => Nada
        case Just(a) => Just(f(a))
      }
  }

  case class ConsOpt(head: Int, tail: Opt[ConsOpt]) {
    def copy(head: Int = head, tail: Opt[ConsOpt] = tail): ConsOpt = {
      ConsOpt.copyCount = ConsOpt.copyCount + 1
      ConsOpt(head, tail)
    }
  }
  object ConsOpt {
    var copyCount = 0
    def fromList(list: List[Int]): Opt[ConsOpt] = list match {
      case Nil => Nada
      case head :: tail =>
        Just(ConsOpt(head, fromList(tail)))
    }
  }
}
