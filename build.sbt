import Dependencies._
import Resolvers._
import BuildSettings._

lazy val buildSettings = Seq(
    organization := Organization,
    name := Name,
    version := Version,
    scalaVersion := VersionScala
)

javacOptions ++= Seq("-encoding", "UTF-8")

lazy val main = (project in file("."))
    .settings(
        buildSettings,
        resolvers := commonResolvers,
        libraryDependencies ++= commonDependencies,
        mainClass in assembly := Some("game_theory.Boot")
    )