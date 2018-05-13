package serize
package test

import AnnotatedPickler._
@serize("CaseObject")case object CaseObject extends SerizeMessage
@serize("CaseClass0")case class CaseClass0() extends SerizeMessage
@serize("CaseClass1")case class CaseClass1(@id(0)a: Int) extends SerizeMessage
@serize("CaseClass2")case class CaseClass2(@id(-93)a: Int, @id(954)b: Int) extends SerizeMessage
@serize("CaseClass2WithDefaults")case class CaseClass2WithDefaults(@id(-93)a: Int = 10, @id(954)b: Int = 20) extends SerizeMessage