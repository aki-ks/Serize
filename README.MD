# Serize
[![](https://jitpack.io/v/aki-ks/serize.svg)](https://jitpack.io/#aki-ks/serize)

Serize is a fork of [boopickle](https://github.com/suzaku-io/boopickle) that provides an advanced macro for pickling case classes.
Data should still be able to be unpickled after the case class/case object was changed:

- Fields and classes can be renamed
- Fields can be added when a default value is defined
- Fields can be removed
- The order of fields can be changed
- Case objects can be turned into case classes and vice versa
- Field types can be changed from concrete case classes to traits/superclasses

# Usage
Add the jitpack resolver and this repository as dependency to your project:
https://jitpack.io/#aki-ks/serize

If you want to use the @serize macro annotation, you must add the macroparadise compiler plugin:
``` sbt
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
```

First of all you must import all serize classes and create an object that inherits all default implicit picklers.
``` Scala
import serize._

object ExamplePicklers extends Default {
  val container = Container
}
```

Ids must be assigned to case objects, case classes and their fields.
They are required for (de)serialization and e.g. to allow renaming of fields.

The mapping between classes and their ids is stored in a `serize.Container`.
It must contain all classes that should be (un)pickled.

Pickable classes must extend the trait `serize.SerizeMessage`.
 The @serize macro will add it for you.

The assignment of ids can be done in two ways:
* via annotations in the declaration
``` Scala
// When using the @serize annotation you must always import your picklers
import ExamplePicklers._

trait Gender
@serize("Gender.Male")case object Male extends Gender
@serize("Gender.Female")case object Female extends Gender

@serize("Person")case class Person(@id(0)name: String, @id(1)age: Int, @id(2)gender: Gender)

val container = Container
  .withCaseObject(Male)
  .withCaseObject(Female)
  .withCaseClass[Person]
```

* or in the Container
``` Scala
trait Gender
case object Male extends Gender with SerizeMessage
case object Female extends Gender with SerizeMessage

case class Person(name: String, age: Int, gender: Gender) extends SerizeMessage

val container = Container
  .withCaseObject(Male, "Gender.Male")
  .withCaseObject(Female, "Gender.Female")
  .withCaseClass[Person]("Person")(F(0).name, F(1).age, F(2).gender)
```

## Adding a fields to case classes

When you add a new field and old serialized data should not become invalid, you must assign a default value.

* Defining default values in class definition:
``` Scala
case class Person(name: String, age: Int, gender: Gender, email: Option[String] = None) extends SerizeMessage
```

* Defining default values in the `Container`:
``` Scala
val container = Container
  .withCaseClass[Person]("Person")(F(0).name, F(1).age, F(2).gender, F(3).email = None)
```

## (Un)pickling data

Since serize is a fork of boopickle pickling and unpickling works the same way.

``` Scala
import ExamplePicklers._

val value = Person("Max", 21, Male)
val serialized: Array[Byte] = Pickle.intoBytes(value).toByteArray
```

``` Scala
import ExamplePicklers._

val bytes: Array[Byte] = ???
val deserialized: Person = Unpickle[Person].fromBytes(bytes.toByteBuffer)
```

# FAQ

**"Can I use serize with akka"?**

Yes, serize was made for usage with [akka](https://akka.io/):

https://github.com/aki-ks/Serize-Akka

**"When using the @serize annotation the compiler cannot find implicit Picklers"**

You have probably not imported your singleton that contains all implicit picklers.
In the above example you've forgotten to add:
``` Scala
import ExamplePicklers._
```

**"The compiler complains about an \"unrecoverable cycle resolving import\" when importing the singleton containing the implicit picklers"**

This error is common if you declare your case classes and the singleton containing the picklers in the same source file.
Move them into separate source files.