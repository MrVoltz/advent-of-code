scalaVersion := "3.3.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

Compile / scalaSource := baseDirectory.value

Test / scalaSource := baseDirectory.value
