package serize
package macros

import scala.reflect.macros._

object WithCaseClassMacro {
  def impl[C : c.WeakTypeTag](c: blackbox.Context)(name: c.Expr[String])(fields: c.Expr[F]*): c.Expr[Container] = {
    import c.universe._

    val classType = weakTypeOf[C]
    val classSymbol = classType.typeSymbol.asClass
    val Apply(Apply(TypeApply(_, List(classNameTree)), _), _) = c.macroApplication

    val parameters = {
      val paramLists = classSymbol.primaryConstructor.asMethod.paramLists
      if (paramLists.length > 1)
        c.abort(c.enclosingPosition, "Case classes with multiple parameter-lists are not supported")
      paramLists.head map (_.asTerm)
    }

    val caseFields = fields.map(f => parseFieldExpression(c)(f))

    requireNoDuplicatedFieldIds(c)(caseFields)
    requireExistanceofFields(c)(caseFields, parameters)
    requireNoDuplicatedFieldNames(c)(caseFields)

    val className = name.tree match {
      case Literal(Constant(name: String)) => name
      case _ => c.abort(name.tree.pos, "Classname must be a string literal")
    }

    val fieldInfos = for((parameter, index) ← parameters.zipWithIndex) yield {
      val fieldName = parameter.name.decodedName.toString
      val fieldType = q"${parameter.typeSignatureIn(classType)}"
      val caseField = caseFields.find(_.name == fieldName).getOrElse {
        c.abort(c.enclosingPosition, s"No metadata for field $fieldName provided")
      }

      val defaultValue = caseField.default orElse {
        if(parameter.isParamWithDefault) Some(invokeDefaultValueGetter(c)(classSymbol, index))
        else None
      }

      new FieldInfo[c.universe.type](caseField.id, fieldType, parameter.name.toTermName, defaultValue, caseField.pos)
    }

    val classInfo = new CaseClassInfo[c.universe.type](className, classNameTree, fieldInfos)

    val pickler = PicklerGenerator.caseClassPickler(c)(classInfo)
    c.Expr[Container](q"""${c.prefix}.withCaseClass[${classNameTree}]($pickler)""")
  }

  def invokeDefaultValueGetter(c: blackbox.Context)(clazz: c.universe.ClassSymbol, paramIndex: Int): c.universe.Tree = {
    import c.universe._
    val getterName = TermName("$lessinit$greater$default$" + (paramIndex + 1))
    val moduleSymbol = clazz.companion

    q"""$moduleSymbol.$getterName"""
  }

  case class CaseField[U <: Universe](id: Int, name: String, pos: U#Position, default: Option[U#Tree])

  def parseFieldExpression(c: blackbox.Context)(expr: c.Expr[F]): CaseField[c.universe.type] = {
    import c.universe._
    val tree = expr.tree
    tree match {
      case q"""${_}.apply(${Literal(Constant(id: Int))}).selectDynamic(${Literal(Constant(name: String))})""" =>
        CaseField[c.universe.type](id, name, tree.pos, None)

      case q"""${_}.apply(${Literal(Constant(id: Int))}).updateDynamic[$typ](${Literal(Constant(name: String))})($default)""" =>
        CaseField[c.universe.type](id, name, tree.pos, Some(default))

      case _ => c.abort(expr.tree.pos, "Illegal field expression")
    }
  }

  def requireNoDuplicatedFieldIds(c: blackbox.Context)(fields: Seq[CaseField[c.universe.type]]) : Unit = {
    val duplicates = fields.groupBy(_.id) flatMap {
      case (id, Seq(field)) => Nil
      case (id, fields) => fields
    }

    if(!duplicates.isEmpty) {
      def msg(id: Int) = s"Duplicated field id $id"

      for(field ← duplicates.tail)
        c.error(field.pos, msg(field.id))

      val firstField = duplicates.head
      c.abort(firstField.pos, msg(firstField.id))
    }
  }

  def requireExistanceofFields(c: blackbox.Context)(fields: Seq[CaseField[c.universe.type]], parameters: Seq[c.universe.Symbol]) : Unit = {
    val fieldNames = parameters.map(_.name.decodedName.toString).toSet

    for(field ← fields) {
      if(!fieldNames.contains(field.name)) {
        c.error(field.pos, s"Case class contains no field named ${field.name}")
      }
    }
  }

  def requireNoDuplicatedFieldNames(c: blackbox.Context)(fields: Seq[CaseField[c.universe.type]]) : Unit = {
    val duplicates = fields.groupBy(_.name) flatMap {
      case (_, fields) if fields.size > 1 => fields
      case _ => Nil
    }

    if(!duplicates.isEmpty) {
      def msg(name: String) = s"Duplicated field name $name"

      for(field ← duplicates.tail)
        c.error(field.pos, msg(field.name))

      val firstField = duplicates.head
      c.abort(firstField.pos, msg(firstField.name))
    }
  }
}
