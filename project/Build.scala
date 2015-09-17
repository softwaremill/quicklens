import org.scalajs.sbtplugin.ScalaJSPlugin.AutoImport.crossProject
import org.scalajs.sbtplugin.cross.CrossType
import sbt.Keys._
import sbt._

object BuildSettings {
  val buildSettings = Defaults.coreDefaultSettings ++ Seq (
    organization  := "com.softwaremill.quicklens",
    version       := "1.4.1",
    scalaVersion  := "2.11.6",
    // Sonatype OSS deployment
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    credentials   += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    pomExtra :=
      <scm>
        <url>git@github.com:adamw/quicklens.git</url>
        <connection>scm:git:git@github.com:adamw/quicklens.git</connection>
      </scm>
        <developers>
          <developer>
            <id>adamw</id>
            <name>Adam Warski</name>
            <url>http://www.warski.org</url>
          </developer>
        </developers>,
    licenses      := ("Apache2", new java.net.URL("http://www.apache.org/licenses/LICENSE-2.0.txt")) :: Nil,
    homepage      := Some(new java.net.URL("http://www.softwaremill.com"))
  )
}

object Dependencies {
  val scalatest     = "org.scalatest" %% "scalatest"  % "2.2.1"       % "test"
}

object QuicklensBuild extends Build {
  import BuildSettings._
  import Dependencies._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(publishArtifact := false)
  ) aggregate(quicklensJVM, quicklensJS, tests)

  lazy val quicklens = (crossProject.crossType(CrossType.Pure) in file("quicklens")).
    settings(
      buildSettings ++ Seq(
        name := "quicklens",
        libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)): _*
    )

  lazy val quicklensJVM = quicklens.jvm
  lazy val quicklensJS = quicklens.js

  lazy val tests: Project = Project(
    "tests",
    file("tests"),
    settings = buildSettings ++ Seq(
      publishArtifact := false,
      libraryDependencies ++= Seq(scalatest),
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "test"),
      // Otherwise when running tests in sbt, the macro is not visible
      // (both macro and usages are compiled in the same compiler run)
      fork in Test := true)
  ) dependsOn(quicklensJVM)
}
