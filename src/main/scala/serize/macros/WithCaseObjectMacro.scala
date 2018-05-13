package serize
package macros

import scala.reflect.macros._

object WithCaseObjectMacro {
  def impl[M : c.WeakTypeTag](c: blackbox.Context)(m: c.Expr[M], name: c.Expr[String]): c.Expr[Container] = {
    import c.universe._

    val moduleType = weakTypeOf[M]
    val moduleName = moduleType.typeSymbol.name

    val moduleId = name.tree match {
      case Literal(Constant(name: String)) => name
      case _ => c.abort(name.tree.pos, "The name must be a string literal")
    }

    val caseObjectInfo = new CaseObjectInfo[c.universe.type](moduleId, moduleName.toTermName)
    val pickler = PicklerGenerator.caseObjectPickler(c)(caseObjectInfo)

    c.Expr[Container](q"""${c.prefix}.withCaseObject[${moduleType}]($m)($pickler)""")
  }
}
