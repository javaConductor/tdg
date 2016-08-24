name := "tdg"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-lang.modules"  %% "scala-parser-combinators" % "1.0.4",
  "org.scala-lang" % "scala-actors" % "2.11.8",
  "com.typesafe.akka" % "akka-actor_2.10" % "2.3.11",
  "log4j" % "log4j" % "1.2.14",
  "org.scala-stm" %% "scala-stm" % "0.7",
  "io.spray" % "spray-can" % "1.3.1"
)
