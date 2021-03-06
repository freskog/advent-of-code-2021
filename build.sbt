name := "advent-of-code-2021"

version := "0.0.1"
scalaVersion := "2.13.7"
scalacOptions += ("-deprecation")
val zioVersion = "2.0.0-RC1"


libraryDependencies ++= Seq(
  "dev.zio" %% "zio"          % zioVersion,
  "dev.zio" %% "zio-streams"  % zioVersion,
  "com.lihaoyi" %% "fastparse" % "2.2.2",
  "dev.zio" %% "zio-test"     % zioVersion % "test",
  "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
)
testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))

addCommandAlias("com", "all compile test:compile")
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
