ThisBuild / scalaVersion := "2.13.16"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

lazy val dependencies = Seq(
    "org.typelevel" %% "cats-core" % "2.13.0"
  , "org.scala-graph" %% "graph-core" % "2.0.0"
  , "org.scalatest" %% "scalatest" % "3.1.2" % Test
  , "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"
)

lazy val root = (project in file("."))
  .settings(
    name := "probprog4s",
    organization := "com.acme",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-Wunused:imports",
    ),
    exportJars := true,
    libraryDependencies ++= dependencies,
  )
    
