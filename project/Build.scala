import sbt._
import Keys._

object ApplicationBuild extends Build {

  val playVersion = "2.1-RC1"

  val main = Project("iteratees-extras", new File(".")).settings(
    libraryDependencies ++= Seq(
      "play" %% "play" % playVersion,
      "org.specs2" % "specs2_2.10.0-RC1" % "1.12.2" %  "test"
    ),
    organization := "play.extras",
    scalaVersion := "2.10.0-RC1"
  )

}
