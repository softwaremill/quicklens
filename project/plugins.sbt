val scalaJSVersion = Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.17.0")
val scalaNativeVersion = Option(System.getenv("SCALANATIVE_VERSION")).getOrElse("0.5.6")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)
addSbtPlugin("org.scala-native" % "sbt-scala-native" % scalaNativeVersion)
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.10.1")
addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.2")

val sbtSoftwareMillVersion = "2.0.20"
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-common" % sbtSoftwareMillVersion)
addSbtPlugin("com.softwaremill.sbt-softwaremill" % "sbt-softwaremill-publish" % sbtSoftwareMillVersion)
