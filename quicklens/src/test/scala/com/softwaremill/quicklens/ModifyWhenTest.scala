package com.softwaremill.quicklens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object ModifyWhenTestData {
  trait Animal
  case class Dog(age: Int) extends Animal
  case class Cat(ages: List[Int]) extends Animal
  case class Zoo(animals: List[Animal])

  val dog: Animal = Dog(4)
  val olderDog: Animal = Dog(5)

  val cat: Animal = Cat(List(3, 12, 13))
  val olderCat: Animal = Cat(List(4, 12, 13))

  val zoo = Zoo(List(dog, cat))
  val olderZoo = Zoo(List(olderDog, olderCat))

  trait MyOption[+A]
  case class MySome[+A](value: A) extends MyOption[A]
  case object MyNone extends MyOption[Nothing]

  val someDog: MyOption[Dog] = MySome(Dog(4))
  val someOlderDog: MyOption[Dog] = MySome(Dog(5))
  val noDog: MyOption[Dog] = MyNone
}

class ModifyWhenTest extends AnyFlatSpec with Matchers {
  import ModifyWhenTestData._

  it should "modify a field in a subtype" in {
    dog.modify(_.when[Dog].age).using(_ + 1) shouldEqual olderDog
  }

  it should "ignore subtypes other than the selected one" in {
    cat.modify(_.when[Dog].age).using(_ + 1) shouldEqual cat
  }

  it should "modify a Functor field in a subtype" in {
    cat.modify(_.when[Cat].ages.at(0)).using(_ + 1) shouldEqual olderCat
  }

  it should "modify a field in a subtype through a Functor" in {
    zoo
      .modifyAll(
        _.animals.each.when[Dog].age,
        _.animals.each.when[Cat].ages.at(0)
      )
      .using(_ + 1) shouldEqual olderZoo
  }

  it should "modify a field in a subtypes (parameterized)" in {
    someDog.modify(_.when[MySome[Dog]].value.age).using(_ + 1) shouldEqual someOlderDog
  }

  it should "ignore subtypes other than the selected one (parameterized)" in {
    noDog.modify(_.when[MySome[Dog]].value.age).using(_ + 1) shouldEqual MyNone
  }
}
