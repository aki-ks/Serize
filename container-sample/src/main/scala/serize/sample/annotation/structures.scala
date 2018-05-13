package serize
package sample
package annotation

import ExamplePicklers._

trait Gender
case object Male extends Gender with SerizeMessage
case object Female extends Gender with SerizeMessage

case class Person(name: String, age: Int, gender: Gender, email: Option[String]) extends SerizeMessage
