import sbt._
import Keys._

object Build extends sbt.Build {
  import BuildSettings._
  import Dependencies._

  val resolutionRepos = Seq(
    "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
  )

  scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")
  val buildSettings = Defaults.defaultSettings 


  lazy val parent = Project(id = "macros-playground",
    base = file("."))
    .aggregate (core, playground)
    .settings(basicSettings: _*)
    

  lazy val core = Project(id = "core", base = file("core"))
    .settings(coreSettings: _*)
    .settings(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _))
    .settings(
      libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core" % "7.0.4"))
    .settings(
      addCompilerPlugin(
        "org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full))
    
  lazy val playground = Project(id = "playground", base = file("playground"))
    .settings(playgroundSettings: _*)
    .dependsOn(core)
}
