package serize
package sample
package annotation

import java.nio.ByteBuffer

object Main extends App {
  import ExamplePicklers._

  val max = Person("Max", 21, Male, None)

  val serialized: ByteBuffer = Pickle.intoBytes(max)

  val array = new Array[Byte](serialized.remaining)
  serialized.mark
  serialized.get(array)
  serialized.reset

  val deserialized: Person = Unpickle[Person].fromBytes(serialized)

  println(s"max: $max")
  println(s"serialized: ${array.toList}")
  println(s"deserialized: $deserialized")
}

object ExamplePicklers extends Default {
  val container = Container
    .withCaseObject(Male, "Gender.Male")
    .withCaseObject(Female, "Gender.Female")
    .withCaseClass[Person]("Person")(F(0).name, F(1).age, F(2).gender, F(3).email = None)
}
