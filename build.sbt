name := "iteratees-extras"
organization := "com.typesafe.play.extras"

scalaVersion := "2.11.8"
crossScalaVersions := Seq("2.11.8")
javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint:unchecked", "-encoding", "UTF-8")
scalacOptions += "-deprecation"

libraryDependencies ++= Seq(
  "com.typesafe.play"         %% "play"               % "2.5.3"     % "provided",
  "org.specs2"                %% "specs2-core"        % "3.3.1"     % "test"
)

resolvers ++= Seq(
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
)

pomExtra := {
  <scm>
    <url>https://github.com/jroper/play-iteratees-extras</url>
    <connection>scm:git:git@github.com:jroper/play-iteratees-extras.git</connection>
  </scm>
  <developers>
    <developer>
      <id>jroper</id>
      <name>James Roper</name>
      <url>https://jazzy.id.au</url>
    </developer>
  </developers>
}
pomIncludeRepository := { _ => false }
homepage := Some(url(s"https://github.com/jroper/play-iteratees-extras"))
licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))

sonatypeProfileName := "com.typesafe"
releasePublishArtifactsAction := PgpKeys.publishSigned.value
releaseTagName := s"${(version in ThisBuild).value}"
releaseCrossBuild := true

import ReleaseTransformations._
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  publishArtifacts,
  releaseStepCommand("sonatypeRelease"),
  setNextVersion,
  commitNextVersion,
  pushChanges
)

