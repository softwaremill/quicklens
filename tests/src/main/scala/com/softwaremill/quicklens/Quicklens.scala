package com.softwaremill.quicklens

object Quicklens extends App {
  case class Street(name: String)
  case class Address(street: Street)
  case class Person(address: Address)

  val person = Person(Address(Street("s1")))

  def changeName(n: String) = n+n

  //

  println(modify(person)(_.address.street.name).using(changeName))
}
