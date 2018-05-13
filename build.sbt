name := "Serize"
organization := "me.aki.serize"
version := "0.1"

scalaVersion := "2.12.6"

def macroparadise = "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full

lazy val root = (project in file("."))
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.6",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
    addCompilerPlugin(macroparadise)
  )

lazy val annotationSample = (project in file("annotation-sample"))
  .dependsOn(root)
  .settings(addCompilerPlugin(macroparadise))

lazy val containerSample = (project in file("container-sample"))
  .dependsOn(root)
  .settings(addCompilerPlugin(macroparadise))
