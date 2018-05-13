package serize.macros

import scala.reflect.macros._

case class CaseObjectInfo[U <: Universe](id: String, name: U#TermName)
case class CaseClassInfo[U <: Universe](id: String, name: U#Tree, fields: List[FieldInfo[U]])
case class FieldInfo[U <: Universe](id: Int, typ: U#Tree, name: U#TermName, default: Option[U#Tree], pos: U#Position)

object PicklerGenerator {
  def caseClassPickler(c: blackbox.Context)(caseClass: CaseClassInfo[c.universe.type]): c.universe.Tree = {
    import c.universe._

    val className = caseClass.name
    val fields = caseClass.fields

    val pickleFunction = {
      val pickleFields = for {
        field <- fields
        expr ← List(
          q"""state.enc.writeInt(${field.id})""",
          q"""state.enc.writeFrame[${field.typ}](value.${field.name})""")
      } yield expr

      q"""
        val ref = state.identityRefFor(value)
        if(ref.isDefined) {
          state.enc.writeInt(-ref.get)
        } else {
          state.enc.writeInt(${fields.size})
          ..$pickleFields
          state.addIdentityRef(value)
        }
      """
    }

    val unpickleFunction = {
      def numberToName(i: Int) = if(i < 0) s"Minus${-i}" else i
      def wasUnpickledName(i: Int) = TermName("wasUnpickled" + numberToName(i))
      def unpickleValueName(i: Int) = TermName("unpickledValue" + numberToName(i))

      val initializeValueFields = for {
        field ← fields
        expr ← List(
          q"""var ${wasUnpickledName(field.id)}: Boolean = false""",
          q"""var ${unpickleValueName(field.id)}: ${field.typ} = null.asInstanceOf[${field.typ}]""")
      } yield expr

      val decodeMatch = {
        val cases = for (field ← fields) yield {
          val body =
            q"""
              ${unpickleValueName(field.id)} = state.dec.readFrame[${field.typ}]
              ${wasUnpickledName(field.id)} = true
            """

          CaseDef(Literal(Constant(field.id)), EmptyTree, body)
        }

        val defaultCase = CaseDef(Ident(termNames.WILDCARD), EmptyTree, q"""state.dec.skipFrame""")
        Match(Ident(TermName("fieldId")), cases :+ defaultCase)
      }

      val fieldValues = for(field ← fields) yield {
        val id = field.id
        val defaultValue = field.default getOrElse {
          val message = s"No default value provided for field with id ${id} (${field.name}) in case class ${className}"
          q"""throw new _root_.serize.NoDefaultValueException($message)"""
        }

        q"""if(${wasUnpickledName(id)}) ${unpickleValueName(id)} else { $defaultValue }"""
      }

      q"""
        val ic = state.dec.readInt
        if(ic < 0) {
          state.identityFor[$className](-ic)
        } else {
          ..${initializeValueFields}

          var i = 0
          while(i < ic) {
            val fieldId = state.dec.readInt
            ${decodeMatch}
            i += 1
          }

          val value = new $className(..$fieldValues)
          state.addIdentityRef(value)
          value
        }
      """
    }

    q"""
      new _root_.serize.CasePickler[$className] {
        override val id: String = ${caseClass.id}
        override def typ: Class[$className] = classOf[$className]
        override def pickleFields(value: $className)(implicit state: _root_.boopickle.PickleState): Unit = $pickleFunction
        override def unpickleData(implicit state: _root_.boopickle.UnpickleState): $className = $unpickleFunction
      }
    """
  }

  def caseObjectPickler(c: blackbox.Context)(module: CaseObjectInfo[c.universe.type]) : c.universe.Tree = {
    import c.universe._
    val moduleName = module.name
    q"""
      new _root_.serize.CasePickler[$moduleName.type] {
        override val id: String = ${module.id}
        override def typ: Class[$moduleName.type] = $moduleName.getClass().asInstanceOf[Class[$moduleName.type]]

        override def pickleFields(value: $moduleName.type)(implicit state: _root_.boopickle.PickleState): Unit = {
          state.enc.writeInt(0)
        }

        override def unpickleData(implicit state: _root_.boopickle.UnpickleState): $moduleName.type = {
          val ic = state.dec.readInt
          if(ic > 0) {
            var i = 0
            while(i < ic) {
              val fieldId = state.dec.readInt
              state.dec.skipFrame
              i += 1
            }
          }
          $moduleName
        }
      }
    """
  }

}
