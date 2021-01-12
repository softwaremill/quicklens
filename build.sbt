val scala211 = "2.11.12"
val scala212 = "2.12.12"
val scala213 = "2.13.4"

val buildSettings = Seq(
  organization := "com.softwaremill.quicklens",
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

val versionSpecificScalaSource = {
  unmanagedSourceDirectories in Compile := {
    val current = (unmanagedSourceDirectories in Compile).value
    val sv = (scalaVersion in Compile).value
    val baseDirectory = (scalaSource in Compile).value
    val versionSpecificSources =
      new File(baseDirectory.getAbsolutePath + "-" + (if (CrossVersion.partialVersion(sv) == Some((2, 13))) "2.13+" else "2.13-"))
    versionSpecificSources +: current
  }
}

lazy val quicklens = (projectMatrix in file("quicklens"))
  .settings(buildSettings)
  .settings(
    name := "quicklens",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
    Test / publishArtifact := false,
    libraryDependencies ++= Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value % Test),
    versionSpecificScalaSource,
    libraryDependencies ++= Seq("flatspec", "shouldmatchers").map(m =>
      "org.scalatest" %%% s"scalatest-$m" % "3.2.3" % Test
    )
  )
  .jvmPlatform(
    scalaVersions = List(scala211, scala212, scala213)
  )
  .jsPlatform(
    scalaVersions = List(scala212, scala213)
  )
  .nativePlatform(
    scalaVersions = List(scala211),
    settings = Seq(
      libraryDependencies ++= Seq(
        "org.scala-native" %%% "test-interface" % "0.4.0-M2" % Test
      ),
      nativeLinkStubs := true
    )
  )
