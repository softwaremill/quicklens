package com.softwaremill.quicklens

object Quicklens extends App {
  case class Street(name: String)
  case class Address(street: Street)
  case class Person(address: Address)

  val person = Person(Address(Street("s1")))

  def changeName(n: String) = n+n

  //

  println(modify(person)(_.address.street.name).using(changeName))

  //

  val modifyPA = modify(_: Person)(_.address)
  modifyPA(person).using(identity)

  //

  val modifyASt = modify(_: Address)(_.street.name)

  println((modifyPA andThenModify modifyASt)(person).using(changeName))
}
