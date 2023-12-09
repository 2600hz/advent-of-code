val scala3Version = "3.3.1"
val AkkaVersion = "2.9.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Advent of code 2023",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    resolvers += "Akka library repository".at("https://repo.akka.io/maven"),

    libraryDependencies += ("org.scalameta" %% "munit" % "0.7.29" % Test),

    libraryDependencies ++= Seq(
      "com.lightbend.akka" %% "akka-stream-alpakka-file" % "7.0.1",
      "com.typesafe.akka" %% "akka-stream" % AkkaVersion
    )
  )
