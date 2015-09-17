Quicklens
=========

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.softwaremill.quicklens/quicklens_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.softwaremill.quicklens/quicklens_2.11)

**Modify deeply nested fields in case classes:**

````scala
import com.softwaremill.quicklens._

case class Street(name: String)
case class Address(street: Street)
case class Person(address: Address, age: Int)

val person = Person(Address(Street("1 Functional Rd.")), 35)

val p2 = person.modify(_.address.street.name).using(_.toUpperCase)
val p3 = person.modify(_.address.street.name).setTo("3 OO Ln.")

// or
 
val p4 = modify(person)(_.address.street.name).using(_.toUpperCase)
val p5 = modify(person)(_.address.street.name).setTo("3 OO Ln.")
````

**Chain modifications:**

````scala
person
  .modify(_.address.street.name).using(_.toUpperCase)
  .modify(_.age).using(_ - 1)
````

**Traverse options/lists using .each:**

````scala
import com.softwaremill.quicklens._

case class Street(name: String)
case class Address(street: Option[Street])
case class Person(addresses: List[Address])

val person = Person(List(
  Address(Some(Street("1 Functional Rd."))),
  Address(Some(Street("2 Imperative Dr.")))
))

val p2 = person.modify(_.addresses.each.street.each.name).using(_.toUpperCase)
````

`.each` can only be used inside a `modify` and "unwraps" the container (currently supports `List`s and `Option`s).
You can add support for your own containers by providing an implicit `QuicklensFunctor[C]` with the appropriate
`C` type parameter.

**Modify specific sequence elements using .at:**

````scala
person.modify(_.addresses.at(2).street.each.name).using(_.toUpperCase)
````

Similarly to `.each`, `.at` modifies only the element at the given index. If there's no element at that index,
an `IndexOutOfBoundsException` is thrown.

**Re-usable modifications (lenses):**

````scala
import com.softwaremill.quicklens._

val modifyStreetName = modify(_: Person)(_.address.street.name)

val p3 = modifyStreetName(person).using(_.toUpperCase)
val p4 = modifyStreetName(anotherPerson).using(_.toLowerCase)

//

val upperCaseStreetName = modify(_: Person)(_.address.street.name).using(_.toUpperCase)

val p5 = upperCaseStreetName(person)
````

**Composing lenses:**

````scala
import com.softwaremill.quicklens._

val modifyAddress = modify(_: Person)(_.address)
val modifyStreetName = modify(_: Address)(_.street.name)

val p6 = (modifyAddress andThenModify modifyStreetName)(person).using(_.toUpperCase)
````

---

Similar to lenses ([1](http://eed3si9n.com/learning-scalaz/Lens.html),
[2](https://github.com/julien-truffaut/Monocle)), but without the actual lens creation.

Read [the blog](http://www.warski.org/blog/2015/02/quicklens-modify-deeply-nested-case-class-fields/) for more info.

Available in Maven Central:

````scala
val quicklens = "com.softwaremill.quicklens" %% "quicklens" % "1.4.1"
````

Also available for [Scala.js](http://www.scala-js.org)!