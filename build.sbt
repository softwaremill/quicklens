import com.softwaremill.SbtSoftwareMillCommon.commonSmlBuildSettings
import com.softwaremill.Publish.{updateDocs, ossPublishSettings}
import com.softwaremill.UpdateVersionInDocs

val scala212 = "2.12.21"
val scala213 = "3.9.0"
val scala3 = "3.3.8"

val scalaIdeaVersion = scala3 // the version for which to import sources into intellij

Global / excludeLintKeys ++= Set(ideSkipProject)

commonSmlBuildSettings
ossPublishSettings

organization := "com.softwaremill.quicklens"
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked"
) // useful for debugging macros: "-Ycheck:all", "-Xcheck-macros"
ideSkipProject := (scalaVersion.value != scalaIdeaVersion)

lazy val root =
  rootProject
    .settings(
      publishArtifact := false,
      moduleName := "quicklens-root",
      scalaVersion := scalaIdeaVersion,
      updateDocs := Def.uncached(
        UpdateVersionInDocs(sLog.value, organization.value, version.value, List(file("README.md")))
      )
    )
    .autoAggregate

val versionSpecificScalaSources = {
  Compile / unmanagedSourceDirectories := {
    val current = (Compile / unmanagedSourceDirectories).value
    val sv = (Compile / scalaVersion).value
    val baseDirectory = (Compile / scalaSource).value
    val suffixes = CrossVersion.partialVersion(sv) match {
      case Some((2, 13)) => List("2", "2.13+")
      case Some((2, _))  => List("2", "2.13-")
      case Some((3, _))  => List("3")
      case _             => Nil
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
  .settings(
    name := "quicklens",
    libraryDependencies ++= reflectLibrary(scalaVersion.value),
    Test / publishArtifact := false,
    libraryDependencies ++= compilerLibrary(scalaVersion.value),
    versionSpecificScalaSources,
    libraryDependencies ++= Seq("flatspec", "shouldmatchers").map(m =>
      "org.scalatest" %% s"scalatest-$m" % "3.2.18" % Test
    )
  )
  .jvmPlatform(
    scalaVersions = List(scala212, scala213, scala3),
    settings = Seq(
      scalacOptions ++= (if (ScalaArtifacts.isScala3(scalaVersion.value)) Seq.empty else Seq("-release", "8"))
    )
  )
  .jsPlatform(
    scalaVersions = List(scala212, scala213, scala3)
  )
  .nativePlatform(
    scalaVersions = List(scala212, scala213, scala3)
  )
