val scalaJSVersion = Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.15.0")
val scalaNativeVersion = Option(System.getenv("SCALANATIVE_VERSION")).getOrElse("0.4.16")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)
addSbtPlugin("org.scala-native" % "sbt-scala-native" % scalaNativeVersion)
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.9.2")
addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.2")

val sbtSoftwareMillVersion = "2.0.18"
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-common" % sbtSoftwareMillVersion)
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-publish" % sbtSoftwareMillVersion)
