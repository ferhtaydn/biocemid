package com.ferhtaydn.biocemid.annotators.tfrf

import com.ferhtaydn.biocemid.annotators.AnnotatorConfig

final case class TfrfAnnotatorConfig(beforeAfterCount: Int, mainThreshold: Double,
  smallThreshold: Double, outputFileSuffix: String) extends AnnotatorConfig
