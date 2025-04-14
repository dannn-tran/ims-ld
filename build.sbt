val scala3Version = "3.6.4"

ThisBuild / organization := "com.github.dannn_tran"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := scala3Version
ThisBuild / scalacOptions += "-Wunused:all"

val Http4sVersion = "0.23.30"
val CirceVersion = "0.14.12"
val LogbackVersion = "1.5.18"
val MunitVersion = "1.1.0"
val MunitCatsEffectVersion = "2.0.0"

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .in(file("core"))
  .settings(
    name := "core"
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-generic" % CirceVersion
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-generic" % CirceVersion
    )
  )

lazy val api = project
  .in(file("api"))
  .settings(
    name := "api",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-server" % Http4sVersion,
      "org.http4s" %% "http4s-ember-client" % Http4sVersion,
      "org.http4s" %% "http4s-circe" % Http4sVersion,
      "org.http4s" %% "http4s-dsl" % Http4sVersion,
      "org.tpolecat" %% "skunk-core" % "0.6.3",
      "org.tpolecat" %% "skunk-circe" % "0.6.3",
      "com.github.pureconfig" %% "pureconfig-core" % "0.17.5",
      "org.scalameta" %% "munit" % MunitVersion % Test,
      "org.typelevel" %% "munit-cats-effect" % MunitCatsEffectVersion % Test,
      "ch.qos.logback" % "logback-classic" % LogbackVersion % Runtime
    ),
    Compile / mainClass := Some("imsld.Main")
  )
  .dependsOn(core.jvm)

inThisBuild(
  List(
    scalaVersion := scala3Version,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)
