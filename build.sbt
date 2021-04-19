
enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSBundlerPlugin)
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0")

scalaJSUseMainModuleInitializer := true

name := "LASS"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.5"
// https://mvnrepository.com/artifact/org.typelevel/cats-core
libraryDependencies += "org.typelevel" %%% "cats-core" % "2.5.0"
// https://mvnrepository.com/artifact/org.typelevel/cats-mtl
libraryDependencies += "org.typelevel" %%% "cats-mtl" % "1.1.3"
// https://mvnrepository.com/artifact/com.lihaoyi/fastparse
libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.2"

// https://mvnrepository.com/artifact/org.scala-js/scalajs-dom
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"

//scalajs-react
libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % "1.7.7"
libraryDependencies += "com.github.japgolly.scalajs-react" %%% "extra" % "1.7.7"

//scalacss
libraryDependencies += "com.github.japgolly.scalacss" %%% "core" % "0.7.0"
libraryDependencies += "com.github.japgolly.scalacss" %%% "ext-react" % "0.7.0"

//react
npmDependencies in Compile ++= Seq(
  "react" -> "16.8.6",
  "react-dom" -> "16.8.6")


// copy  javascript files to js folder,that are generated using fastOptJS/fullOptJS
crossTarget in (Compile, fullOptJS) := file("js")
crossTarget in (Compile, fastOptJS) := file("js")
artifactPath in (Compile, fastOptJS) := ((crossTarget in (Compile, fastOptJS)).value /
  ((moduleName in fastOptJS).value + "-opt.js"))


