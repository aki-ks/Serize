package serize
package sample
package annotation

import ExamplePicklers._

trait Gender
@serize("Gender.Male") case object Male extends Gender
@serize("Gender.Female") case object Female extends Gender
@serize("Person") case class Person(@id(0) name: String, @id(1) age: Int, @id(2) gender: Gender)