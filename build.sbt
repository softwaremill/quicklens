val scala211 = "2.11.12"
val scala212 = "2.12.13"
val scala213 = "2.13.5"
val scala3 = "3.0.0-RC3"

val buildSettings = Seq(
  organization := "com.softwaremill.quicklens",
  scalacOptions := Seq("-deprecation", "-feature", "-unchecked"), // useful for debugging macros: "-Ycheck:all"
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
    Developer("adamw", "Adam Warski", "adam@warski.org", url("http://www.warski.org"))
  ),
  scmInfo := Some(
    ScmInfo(
      browseUrl = url("https://github.com/adamw/quicklens"),
      connection = "scm:git:git@github.com:adamw/quicklens.git"
    )
  ),
  licenses := Seq("Apache2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("http://www.softwaremill.com")),
  sonatypeProfileName := "com.softwaremill",
  // sbt-release
  releaseCrossBuild := false,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseIgnoreUntrackedFiles := true,
  releaseProcess := QuicklensRelease.steps
)

lazy val root =
  project
    .in(file("."))
    .settings(buildSettings)
    .settings(publishArtifact := false)
    .aggregate(quicklens.projectRefs: _*)

val versionSpecificScalaSources = {
  Compile / unmanagedSourceDirectories := {
    val current = (Compile / unmanagedSourceDirectories).value
    val sv = (Compile / scalaVersion).value
    val baseDirectory = (Compile / scalaSource).value
    val suffixes = CrossVersion.partialVersion(sv) match {
      case Some((2, 13)) => List("2", "2.13+")
      case Some((2, _)) => List("2", "2.13-")
      case Some((3, _)) => List("3")
    }
    val versionSpecificSources = suffixes.map(s => new File(baseDirectory.getAbsolutePath + "-" + s))
    versionSpecificSources ++ current
  }
}

def compilerLibrary(scalaVersion: String) = {
  if (scalaVersion == scala3) {
    Seq.empty
  } else {
    Seq("org.scala-lang" % "scala-compiler" % scalaVersion % Test)
  }
}

def reflectLibrary(scalaVersion: String) = {
  if (scalaVersion == scala3) {
    Seq.empty
  } else {
    Seq("org.scala-lang" % "scala-reflect" % scalaVersion % Provided)
  }
}

lazy val quicklens = (projectMatrix in file("quicklens"))
  .settings(buildSettings)
  .settings(
    name := "quicklens",
    libraryDependencies ++= reflectLibrary(scalaVersion.value),
    Test / publishArtifact := false,
    libraryDependencies ++= compilerLibrary(scalaVersion.value),
    versionSpecificScalaSources,
    libraryDependencies ++= Seq("flatspec", "shouldmatchers").map(m =>
      "org.scalatest" %%% s"scalatest-$m" % "3.2.8" % Test
    )
  )
  .jvmPlatform(
    scalaVersions = List(scala211, scala212, scala213, scala3)
  )
  .jsPlatform(
    scalaVersions = List(scala212, scala213, scala3)
  )
  .nativePlatform(
    scalaVersions = List(scala211),
    settings = Seq(
      nativeLinkStubs := true
    )
  )