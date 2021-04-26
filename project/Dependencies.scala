import sbt._

object Dependencies {
  private val akkaVersion = "2.5.26"
  private val akkaHttpVersion = "10.1.11"

  val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion
  val akkaStream = "com.typesafe.akka" %% "akka-stream" % akkaVersion
  val akkaSlf4j = "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
  val akkaHttp = "com.typesafe.akka" %% "akka-http" % akkaHttpVersion

  val json4s = "org.json4s" %% "json4s-native" % "3.6.9"

  val log4jApi = "org.apache.logging.log4j" % "log4j-api" % "2.13.3"
  val log4jCore = "org.apache.logging.log4j" % "log4j-core" % "2.13.3"
  val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"

  val poi = "org.apache.poi" % "poi" % "4.1.2"
  val poi_ooxml = "org.apache.poi" % "poi-ooxml" % "4.1.2"
  val doc4j = "org.docx4j" % "docx4j" % "6.1.2" excludeAll ExclusionRule(organization = "org.slf4j")

  val matrixUtils = "com.googlecode.efficient-java-matrix-library" % "ejml" % "0.25"

  def commonDependencies = Seq(
    akkaActor, akkaStream, akkaHttp, akkaSlf4j,
    json4s,
    log4jApi, log4jCore, logback,
    poi, poi_ooxml, doc4j,
    matrixUtils
  )
}
