name := "pure-fp-datastructures"

scalaVersion := "2.12.9"

scalacOptions += "-Ypartial-unification"

val catsVersion = "2.0.0-M4"
val scalaTestVersion = "3.0.8"
val scalaCheckVersion = "1.14.0"

libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test"
)
