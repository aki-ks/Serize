package serize

import scala.annotation.{StaticAnnotation, compileTimeOnly}

import serize.macros.SerizeAnnotationMacro

/** Macro annotation that generated an implicit CasePickler for case classes and case objects */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class serize(name: String) extends StaticAnnotation {
  import scala.language.experimental.macros
  def macroTransform(annottees: Any*): Any = macro SerizeAnnotationMacro.impl
}

/** Set the id of a field in a case class */
class id(id: Int) extends StaticAnnotation
