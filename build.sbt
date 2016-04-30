import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

name := "biocemid"

organization := "com.ferhtaydn"

version := "0.1.0"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.10.6", "2.11.8")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
  "com.typesafe" % "config" % "1.3.0",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0",
  "ch.qos.logback" % "logback-classic" % "1.1.7"
)

mainClass in (Compile, run) := Some("com.ferhtaydn.biocemid.Main")

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yinline-warnings",
  "-Ywarn-dead-code",
  "-Xfuture")

SbtScalariform.scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(RewriteArrowSymbols, true)