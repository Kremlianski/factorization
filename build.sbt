enablePlugins(ScalaJSPlugin)

name := "NumbersLib"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies += "io.monix" %%% "monix" % "2.3.0"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"


scalaJSUseMainModuleInitializer := true

