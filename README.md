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

**Modify conditionally:**

````scala
person.modify(_.address.street.name).setToIfDefined(Some("3 00 Ln."))
person.modify(_.address.street.name).setToIf(shouldChangeAddress)("3 00 Ln.")
````

**Modify several fields in one go:**

````scala
import com.softwaremill.quicklens._

case class Person(firstName: String, middleName: Option[String], lastName: String)

val person = Person("john", Some("steve"), "smith")

person.modifyAll(_.firstName, _.middleName.each, _.lastName).using(_.capitalize)
````

**Traverse options/lists/maps using .each:**

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

`.each` can only be used inside a `modify` and "unwraps" the container (currently supports `List`s, `Option`s and
`Maps`s - only values are unwrapped for maps).
You can add support for your own containers by providing an implicit `QuicklensFunctor[C]` with the appropriate
`C` type parameter.

**Traverse selected elements using .eachWhere:**

Similarly to `.each`, you can use `.eachWhere(p)` where `p` is a predicate to modify only the elements which satisfy
the condition. All other elements remain unchanged.

````scala
def filterAddress: Address => Boolean = ???
person
  .modify(_.addresses.eachWhere(filterAddress)
           .street.eachWhere(_.name.startsWith("1")).name)
  .using(_.toUpperCase)
````

**Modify specific sequence elements using .at:**

````scala
person.modify(_.addresses.at(2).street.each.name).using(_.toUpperCase)
````

Similarly to `.each`, `.at` modifies only the element at the given index. If there's no element at that index,
an `IndexOutOfBoundsException` is thrown.

**Modify specific map elements using .at:**

````scala
case class Property(value: String)

case class Person(name: String, props: Map[String, Property])

val person = Person(
  "Joe",
  Map("Role" -> Property("Programmmer"), "Age" -> Property("45"))
)

person.modify(_.props.at("Age").value).setTo("45")
````

Similarly to `.each`, `.at` modifies only the element with the given key. If there's no such element,
an `NoSuchElementException` is thrown.

**Modify Either fields using .eachLeft and eachRight:**

````scala
case class AuthContext(token: String)
case class AuthRequest(url: String)
case class Resource(auth: Either[AuthContext, AuthRequest])

val devResource = Resource(auth = Left(AuthContext("fake"))

val prodResource = devResource.modify(_.auth.eachLeft.token).setTo("real")

````

**Modify fields when they are of a certain subtype:**

```scala
trait Animal
case class Dog(age: Int) extends Animal
case class Cat(ages: List[Int]) extends Animal

case class Zoo(animals: List[Animal])

val zoo = Zoo(List(Dog(4), Cat(List(3, 12, 13))))

val olderZoo = zoo.modifyAll(
  _.animals.each.when[Dog].age,
  _.animals.each.when[Cat].ages.at(0)
).using(_ + 1)
```

This is also known as a *prism*, see e.g. [here](http://julien-truffaut.github.io/Monocle/optics/prism.html).

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

**Modify nested sealed hierarchies:**

> *Note: this feature is experimental and might not work due to compilation order issues.
> See https://issues.scala-lang.org/browse/SI-7046 for more details.*

````scala
import com.softwaremill.quicklens._

sealed trait Pet { def name: String }
case class Fish(name: String) extends Pet
sealed trait LeggedPet extends Pet
case class Cat(name: String) extends LeggedPet
case class Dog(name: String) extends LeggedPet

val pets = List[Pet](
  Fish("Finn"), Cat("Catia"), Dog("Douglas")
)

val juniorPets = pets.modify(_.each.name).using(_ + ", Jr.")
````

---

Similar to lenses ([1](http://eed3si9n.com/learning-scalaz/Lens.html),
[2](https://github.com/julien-truffaut/Monocle)), but without the actual lens creation.

Read [the blog](http://www.warski.org/blog/2015/02/quicklens-modify-deeply-nested-case-class-fields/) for more info.

Available in Maven Central:

````scala
val quicklens = "com.softwaremill.quicklens" %% "quicklens" % "1.4.8"
````

Also available for [Scala.js](http://www.scala-js.org)!
