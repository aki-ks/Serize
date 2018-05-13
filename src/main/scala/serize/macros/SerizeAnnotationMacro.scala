package serize
package macros

import scala.reflect.macros._

object SerizeAnnotationMacro {
  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    annottees match {
      case Expr(module : ModuleDef) :: Nil =>
        val moduleInfo = {
          val moduleId = annotatedName(c)(c.prefix.tree).get
          CaseObjectInfo[c.universe.type](moduleId, module.name.toTermName)
        }
        val pickler = PicklerGenerator.caseObjectPickler(c)(moduleInfo)
        val newModule = moduleWithSerizeMessageTrait(c)(addToModuleBody(c)(module, picklerField(c)(pickler)))
        c.Expr(q"""$newModule""")

      case Expr(classDef : ClassDef) :: Expr(module: ModuleDef) :: Nil =>
        val classInfo = collectCaseClassInfo(c)(classDef)
        val pickler = PicklerGenerator.caseClassPickler(c)(classInfo)
        val newModule = addToModuleBody(c)(module, picklerField(c)(pickler))
        val newClassDef = classWithSerizeMessageTrait(c)(classDef)

        c.Expr(
          q"""
             $newModule
             $newClassDef
           """)

      case Expr(classDef : ClassDef) :: Nil =>
        val classRep = collectCaseClassInfo(c)(classDef)
        val pickler = PicklerGenerator.caseClassPickler(c)(classRep)
        val newClassDef = classWithSerizeMessageTrait(c)(classDef)

        c.Expr(
          q"""
             object ${classDef.name.toTermName} {
               ${picklerField(c)(pickler)}
             }
             $newClassDef
           """)
    }
  }

  private def picklerField(c: blackbox.Context)(pickler: c.universe.Tree): c.universe.Tree = {
    import c.universe._
    q"""implicit val macroGeneratedCasePickler = $pickler"""
  }

  private def addToModuleBody(c: blackbox.Context)(module: c.universe.ModuleDef, newValue : c.universe.Tree) : c.universe.ModuleDef = {
    import c.universe._

    val oldImpl = module.impl
    val oldBody = oldImpl.body

    val newBody = oldBody :+ newValue
    val newImpl = treeCopy.Template(oldImpl, oldImpl.parents, oldImpl.self, newBody)
    treeCopy.ModuleDef(module, module.mods, module.name, newImpl)
  }

  private def moduleWithSerizeMessageTrait(c: blackbox.Context)(classDef: c.universe.ModuleDef): c.universe.ModuleDef = {
    import c.universe._
    treeCopy.ModuleDef(classDef, classDef.mods, classDef.name, templateWithSerizeMessageTrait(c)(classDef.impl))
  }

  private def classWithSerizeMessageTrait(c: blackbox.Context)(classDef: c.universe.ClassDef): c.universe.ClassDef = {
    import c.universe._
    treeCopy.ClassDef(classDef, classDef.mods, classDef.name, classDef.tparams, templateWithSerizeMessageTrait(c)(classDef.impl))
  }

  private def templateWithSerizeMessageTrait(c: blackbox.Context)(template: c.universe.Template): c.universe.Template = {
    import c.universe._
    val isMarked = template.parents.exists {
      case Ident(TypeName("SerizeMessage")) => true
      case _ => false
    }

    if(isMarked)template
    else {
      val serizeMessageTrait = Select(Select(Ident(termNames.ROOTPKG), TermName("serize")), TypeName("SerizeMessage"))

      val parents = template.parents :+ serizeMessageTrait
      treeCopy.Template(template, parents, template.self, template.body)
    }
  }

  private def collectCaseClassInfo(c: blackbox.Context)(caseClass: c.universe.ClassDef): CaseClassInfo[c.universe.type] = {
    import c.universe._

    if(!caseClass.mods.hasFlag(Flag.CASE))
      c.abort(c.enclosingPosition, "Non-case classes cannot be unpickled")

    val className = Ident(caseClass.name)
    val classId = annotatedName(c)(c.prefix.tree).get

    val constructor = caseClass.impl.body collectFirst {
      case constructor : DefDef if constructor.name == termNames.CONSTRUCTOR => constructor
    } get

    if(constructor.vparamss.length > 1)
      c.abort(c.enclosingPosition, "Case classes with multiple parameter-lists are not supported")

    val fields = for(param ← constructor.vparamss.head) yield {
      val id = extractAnnotatedId(c)(param)
      val defaultValue = param.rhs match {
        case EmptyTree => None
        case value => Some(value)
      }

      FieldInfo[c.universe.type](id, param.tpt, param.name, defaultValue, param.pos)
    }

    requireNoDuplicatedFieldIds(c)(fields)

    CaseClassInfo(classId, className, fields)
  }

  private def requireNoDuplicatedFieldIds(c: blackbox.Context)(fields: List[FieldInfo[c.universe.type]]): Unit = {
    val duplicatedFields = fields groupBy (_.id) flatMap {
      case (id, fields) if fields.length > 1 => fields
      case _ => Nil
    } toList

    if(!duplicatedFields.isEmpty) {
      def msg(id: Int) = s"Duplicated field id $id"

      for(field ← duplicatedFields.tail)c.error(field.pos, msg(field.id))

      val firstField = duplicatedFields.head
      c.abort(firstField.pos, msg(firstField.id))
    }
  }

  private def extractAnnotatedId(c: blackbox.Context)(param: c.universe.ValDef): Int = {
    val ids = for {
      annotation ← param.mods.annotations
      id ← annotatedId(c)(annotation)
    } yield id

    ids match {
      case id :: Nil => id
      case Nil => c.abort(param.pos, "Expected an @id annotation")
      case _ => c.abort(param.pos, "Expected only one @id annotation")
    }
  }

  /** get the id of an @id annotation */
  private def annotatedId(c: blackbox.Context)(anno: c.universe.Tree): Option[Int] = {
    import c.universe._

    val idOpt = anno match {
      case q"""new id(id = $id)""" => Some(id)
      case q"""new id($id)""" => Some(id)
      case _ => None
    }

    idOpt map {
      case Literal(Constant(id: Int)) => id
      case _ => c.abort(anno.pos, "@id annotations must contain a constant id")
    }
  }

  /** get the name of an @serize annotation */
  private def annotatedName(c: blackbox.Context)(anno: c.universe.Tree): Option[String] = {
    import c.universe._

    val idOpt = anno match {
      case q"""new serize(id = $id)""" => Some(id)
      case q"""new serize($id)""" => Some(id)
      case _ => None
    }

    idOpt map {
      case Literal(Constant(name: String)) => name
      case _ => c.abort(anno.pos, "@name annotations must contain a constant id")
    }
  }
}
