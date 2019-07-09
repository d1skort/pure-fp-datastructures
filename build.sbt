name := "pure-fp-datastructures"

scalaVersion := "2.13.0"

scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-language:higherKinds"
)

val catsVersion = "2.0.0-M4"
val scalaTestVersion = "3.0.8"
val scalaCheckVersion = "1.14.0"
val kindProjectorVersion = "0.10.3"

libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test"
)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorVersion)