import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val scala211 = "2.11.12"
val scala212 = "2.12.8"
val scala213 = "2.13.0-RC3"

val buildSettings = Defaults.coreDefaultSettings ++ Seq(
  organization := "com.softwaremill.quicklens",
  scalaVersion := scala212,
  crossScalaVersions := Seq(scalaVersion.value, scala211, scala213),
  scalacOptions := Seq("-deprecation", "-feature", "-unchecked"),
  scalafmtOnCompile := true,
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
  pomIncludeRepository := { _ =>
    false
  },
  developers := List(
    Developer("adamw",
              "Adam Warski",
              "adam@warski.org",
              url("http://www.warski.org"))
  ),
  scmInfo := Some(
    ScmInfo(browseUrl = url("https://github.com/adamw/quicklens"),
            connection = "scm:git:git@github.com:adamw/quicklens.git")
  ),
  licenses := Seq(
    "Apache2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("http://www.softwaremill.com")),
  sonatypeProfileName := "com.softwaremill",
  // sbt-release
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseIgnoreUntrackedFiles := true,
  releaseProcess := QuicklensRelease.steps
)

lazy val root =
  project
    .in(file("."))
    .settings(buildSettings)
    .settings(publishArtifact := false)
    .aggregate(quicklensJVM, quicklensJS, quicklensNative)

lazy val quicklens = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .settings(buildSettings)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    Test / publishArtifact := false,
    libraryDependencies ++= Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value % Test),
    // Adds a `src/main/scala-2.13+` source directory for Scala 2.13 and newer
    // and a `src/main/scala-2.13-` source directory for Scala version older than 2.13
    unmanagedSourceDirectories in Compile += {
      // sourceDirectory returns a platform-scoped directory, e.g. /.jvm
      val sourceDir = (baseDirectory in Compile).value / ".." / "src" / "main"
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n >= 13 => sourceDir / "scala-2.13+"
        case _                       => sourceDir / "scala-2.13-"
      }
    },
  )
  .jvmSettings(
    // Otherwise when running tests in sbt, the macro is not visible
    // (both macro and usages are compiled in the same compiler run)
    Test / fork := true
  )
  .platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.8-RC5" % Test
  )
  .nativeSettings(
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.0-SNAP10" % Test,
    scalaVersion := scala211,
    crossScalaVersions := Seq(scala211),
    nativeLinkStubs := true
  )

lazy val quicklensJVM = quicklens.jvm
lazy val quicklensJS = quicklens.js
lazy val quicklensNative = quicklens.native
