import org.scalajs.linker.interface.ModuleSplitStyle

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
      // "org.http4s" %% "http4s-scalatags" % "0.13.1",
      "org.scalameta" %% "munit" % MunitVersion % Test,
      "org.typelevel" %% "munit-cats-effect" % MunitCatsEffectVersion % Test,
      "ch.qos.logback" % "logback-classic" % LogbackVersion % Runtime
    ),
    Compile / mainClass := Some("imsld.api.Main"),
    ThisBuild / scalacOptions ++= Seq(
      "-Xmax-inlines:64"
    )
  )
  .dependsOn(core.jvm)

lazy val dashboard = project
  .in(file("dashboard"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "dashboard",
    // Tell Scala.js that this is an application with a main method
    scalaJSUseMainModuleInitializer := true,

    /* Configure Scala.js to emit modules in the optimal way to
     * connect to Vite's incremental reload.
     * - emit ECMAScript modules
     * - emit as many small modules as possible for classes in the "dataentry" package
     * - emit as few (large) modules as possible for all other classes
     *   (in particular, for the standard library)
     */
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("dashboard"))
        )
    },
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "org.typelevel" %%% "cats-core" % "2.13.0",
      "com.raquo" %%% "laminar" % "17.2.0",
      "com.raquo" %%% "waypoint" % "9.0.0",
      "be.doeraene" %%% "url-dsl" % "0.6.2",
      "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
      "io.circe" %%% "circe-parser" % CirceVersion,
      "org.scalameta" %%% "munit" % "1.1.0" % Test
    )
  )
  .dependsOn(core.js)

inThisBuild(
  List(
    scalaVersion := scala3Version,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)
