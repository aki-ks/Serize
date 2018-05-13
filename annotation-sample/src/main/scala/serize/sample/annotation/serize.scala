package serize
package sample
package annotation

import java.nio.ByteBuffer

object Main extends App {
  import ExamplePicklers._

  val max = Person("Max", 21, Male)
  val serialized: Array[Byte] = Pickle.intoBytes(max).toByteArray
  val deserialized: Person = Unpickle[Person].fromBytes(serialized.toByteBuffer)

  println(s"max: $max")
  println(s"serialized: ${serialized.toList}")
  println(s"deserialized: $deserialized")
}

object ExamplePicklers extends Default {
  val container = Container
    .withCaseObject(Male)
    .withCaseObject(Female)
    .withCaseClass[Person]
}
