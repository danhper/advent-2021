name := "advent-2021"

version := "0.1"

scalaVersion := "3.1.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"

libraryDependencies += "edu.stanford.nlp" % "stanford-corenlp" % "4.3.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test

Test / logBuffered := false
