val scalaJSVersion = Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.1.0")
val scalaNativeVersion = Option(System.getenv("SCALANATIVE_VERSION")).getOrElse("0.4.0-M2")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.0.0")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.0.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)

addSbtPlugin("org.scala-native" % "sbt-scala-native" % scalaNativeVersion)

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.3.2")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.8.1")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.2")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.13")
