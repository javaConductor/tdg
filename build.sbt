name := "tdg"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-actors" % "2.11.8",
  "com.typesafe.akka" % "akka-actor_2.10" % "2.3.11",
  "log4j" % "log4j" % "1.2.14",
  "io.spray" % "spray-can" % "1.3.1"

)
libraryDependencies += "com.chuusai" % "shapeless_2.12" % "2.3.2"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += ("org.scala-stm" %% "scala-stm" % "0.8")
libraryDependencies += "org.scalaz" % "scalaz-core_2.12" % "7.2.14"

libraryDependencies += "junit" % "junit" % "4.12" % "test"
