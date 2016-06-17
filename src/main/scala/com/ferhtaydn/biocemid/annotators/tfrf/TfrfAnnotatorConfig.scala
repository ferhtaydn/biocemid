package com.ferhtaydn.biocemid.annotators.tfrf

import com.ferhtaydn.biocemid.annotators.AnnotatorConfig

final case class TfrfAnnotatorConfig(rawDirectory: String, beforeAfterCount: Int, mainThreshold: Double,
  smallThreshold: Double, outputFileSuffix: String, useNamedEntity: Boolean = false,
  pureBaseline: Boolean = false, useINO: Boolean = false) extends AnnotatorConfig
