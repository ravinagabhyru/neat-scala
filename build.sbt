name := "neat-scala"

version := "0.0.1"

scalaVersion in ThisBuild := "2.11.8"

lazy val all = (project in file("."))
  .aggregate(api, examples)

lazy val api = (project in file("./api"))
  .settings(moduleName := "neat-scala-api", name := "Neat Api for Scala")
  .settings(sourcesInBase := false)


lazy val examples = (project in file("./examples"))
  .dependsOn(api)
  .settings(moduleName := "neat-scala-examples", name := "Neat for Scala Examples")
