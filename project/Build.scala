import sbt._
import Keys._

object ApplicationBuild extends Build {

  val playVersion = "2.4.1"

  val main = Project("iteratees-extras", new File(".")).settings(
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play" % playVersion % "provided",
      "org.specs2" %% "specs2" % "2.3.12" %  "test"
    ),
    publishTo <<= (version) { version: String =>
      val nexus = "https://private-repo.typesafe.com/typesafe/"
      if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "maven-snapshots/")
      else                                   Some("releases"  at nexus + "maven-releases/")
    },
    organization := "com.typesafe.play.extras",
    version := "1.5.0-SNAPSHOT",
    scalaVersion := "2.11.6",
    crossScalaVersions := Seq("2.11.6", "2.10.4")
  )

}
