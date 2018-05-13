package serize

import scala.language.dynamics

/** Dsl for the macro "Container.withCaseClass" */
object F {
  def apply(id: Int) = new Dynamic {
    def selectDynamic(name: String): F = ???
    def updateDynamic[A](name: String)(value: A): F = ???
  }
}

sealed trait F