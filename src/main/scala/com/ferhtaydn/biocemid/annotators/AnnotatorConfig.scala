package com.ferhtaydn.biocemid.annotators

abstract class AnnotatorConfig {
  val beforeAfterCount: Int
  val mainThreshold: Double
  val smallThreshold: Double
  val outputFileSuffix: String
  val useNamedEntity: Boolean
  val pureBaseline: Boolean
  val useINO: Boolean
}
