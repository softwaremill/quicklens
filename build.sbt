import org.scalajs.sbtplugin.ScalaJSPlugin.AutoImport.crossProject
import org.scalajs.sbtplugin.cross.CrossType

val buildSettings = Defaults.coreDefaultSettings ++ Seq(
  organization := "com.softwaremill.quicklens",
  version := "1.4.9-SNAPSHOT",
  scalaVersion := "2.11.11",
  crossScalaVersions := Seq(scalaVersion.value, "2.12.3"),
  scalacOptions := Seq("-deprecation", "-feature", "-unchecked"),
  // Sonatype OSS deployment
  publishTo := Some(
    if (isSnapshot.value)
      Opts.resolver.sonatypeSnapshots
    else
      Opts.resolver.sonatypeStaging
  ),
  credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  developers := List(
    Developer("adamw", "Adam Warski", "adam@warski.org", url("http://www.warski.org"))
  ),
  scmInfo := Some(
    ScmInfo(browseUrl = url("https://github.com/adamw/quicklens"),
      connection = "scm:git:git@github.com:adamw/quicklens.git")
  ),
  licenses := Seq("Apache2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("http://www.softwaremill.com")),
  sonatypeProfileName := "com.softwaremill",
  // sbt-release
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseIgnoreUntrackedFiles := true
)

lazy val root =
  (project in file("."))
    .settings(buildSettings ++ Seq(publishArtifact := false))
    .aggregate(quicklensJVM, quicklensJS, tests)

lazy val quicklens =
  (crossProject.crossType(CrossType.Pure) in file("quicklens"))
    .settings(buildSettings ++ Seq(
      name := "quicklens",
      libraryDependencies += ("org.scala-lang" % "scala-reflect" % scalaVersion.value))
    )

lazy val quicklensJVM = quicklens.jvm
lazy val quicklensJS = quicklens.js

lazy val tests: Project =
  (project in file("tests"))
    .settings(buildSettings ++
      Seq(
        publishArtifact := false,
        libraryDependencies ++= Seq(
          "org.scalatest" %% "scalatest" % "3.0.4" % "test",
          "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test"),
        // Otherwise when running tests in sbt, the macro is not visible
        // (both macro and usages are compiled in the same compiler run)
        fork in Test := true)
    )
    .dependsOn(quicklensJVM)
