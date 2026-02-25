ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "3.8.2"
ThisBuild / organization := "net.egp"

lazy val zioVersion = "2.1.24"
lazy val upickleVersion = "4.4.3"

lazy val root = (project in file("."))
  .aggregate(cfCore, cfTest)
  .settings(
    name := "scalacf-zio",
    publish / skip := true
  )

lazy val cfCore = (project in file("cf-core"))
  .settings(
    name := "cf-core",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "4.1.0" // if core will parse vectors later, else move to cf-test
    )
  )

lazy val cfTest = (project in file("cf-test"))
  .dependsOn(cfCore)
  .settings(
    name := "cf-test",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-test" % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
      "com.lihaoyi" %% "upickle" % upickleVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),

    Test / javaOptions ++= Seq(
      "-Xmx4G",
      "-XX:+UseG1GC"
    )
  )
