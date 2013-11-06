import sbt._
import Keys._

object BuildSettings {

  lazy val basicSettings = Seq(
    version               := "0.1.0-SNAPSHOT",
    organization          := "org.thikmeta",
    startYear             := Some(2013),
    licenses              := Seq("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
    scalaVersion          := "2.10.3",
    resolvers             ++= Dependencies.resolutionRepos,
    scalacOptions         ++= Seq("-deprecation", "-unchecked", "-feature")
  )

  lazy val coreSettings = basicSettings
  lazy val playgroundSettings = basicSettings
}
