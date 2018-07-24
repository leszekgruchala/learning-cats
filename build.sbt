name := "learning-cats"

version := "1.0"

scalaVersion := "2.12.6"

val catsVersion = "1.1.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % "0.10.1",
  "org.typelevel" %% "cats-testkit" % catsVersion % "test",
  "com.lihaoyi" % "ammonite" % "1.1.2" % "test" cross CrossVersion.full
)

scalacOptions ++= Seq("-feature", "-Ypartial-unification")

initialCommands in console := "import cats._, cats.implicits._"
initialCommands in (Test, console) := """ammonite.Main(predef = "import cats._, cats.implicits._").run()"""
