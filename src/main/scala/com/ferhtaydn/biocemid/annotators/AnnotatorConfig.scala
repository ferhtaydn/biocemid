package com.ferhtaydn.biocemid.annotators

sealed abstract class AnnotatorConfig {
  val beforeAfterCount: Int
  val mainThreshold: Double
  val smallThreshold: Double
}

final case class BaselineAnnotatorConfig(beforeAfterCount: Int, mainThreshold: Double,
  smallThreshold: Double) extends AnnotatorConfig

final case class TfrfAnnotatorConfig(beforeAfterCount: Int, mainThreshold: Double,
  smallThreshold: Double) extends AnnotatorConfig

final case class Word2vecAnnotatorConfig(w2vDir: String, suffix: String, beforeAfterCount: Int,
  mainThreshold: Double, smallThreshold: Double) extends AnnotatorConfig
