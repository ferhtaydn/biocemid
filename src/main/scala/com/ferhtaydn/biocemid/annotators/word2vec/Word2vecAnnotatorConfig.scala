package com.ferhtaydn.biocemid.annotators.word2vec

import com.ferhtaydn.biocemid.annotators.AnnotatorConfig

final case class Word2vecAnnotatorConfig(w2vDir: String, suffix: String, rawDirectory: String, beforeAfterCount: Int,
  mainThreshold: Double, smallThreshold: Double, outputFileSuffix: String,
  useNamedEntity: Boolean = false, pureBaseline: Boolean = false, useINO: Boolean = false) extends AnnotatorConfig
