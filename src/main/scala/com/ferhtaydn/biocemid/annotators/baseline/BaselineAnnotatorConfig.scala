package com.ferhtaydn.biocemid.annotators.baseline

import com.ferhtaydn.biocemid.annotators.AnnotatorConfig

final case class BaselineAnnotatorConfig(beforeAfterCount: Int, mainThreshold: Double, smallThreshold: Double,
  outputFileSuffix: String, useNamedEntity: Boolean = false, pureBaseline: Boolean = false) extends AnnotatorConfig
