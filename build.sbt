name := "Brainfuck"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0"

mainClass in (Compile, packageBin) := Some("Brainfuck3")
mainClass in assembly := Some("Brainfuck3")
