lazy val commonSettings = Seq(
  scalaVersion := "2.11.8"
)

lazy val core = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(spray.boilerplate.BoilerplatePlugin)
  .settings(
    name :="formfactories-core",
    libraryDependencies ++=  Seq(
      "com.lihaoyi" %%% "scalatags" % "0.5.5",
      "org.scala-js" %%% "scalajs-dom" % "0.9.0")
  )
  .settings(commonSettings:_*)

lazy val widgets = project.in(file("widgets")).enablePlugins(ScalaJSPlugin)
  .settings(
    name := "formfactories",
    persistLauncher := true
    )
  .settings(commonSettings:_*)
  .dependsOn(core)
  .aggregate(core)
