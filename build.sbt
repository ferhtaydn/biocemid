import scalariform.formatter.preferences._

name := "BioCreative5Task6"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.4",
  "com.typesafe" % "config" % "1.3.0",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.2"
)


scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(RewriteArrowSymbols, true)