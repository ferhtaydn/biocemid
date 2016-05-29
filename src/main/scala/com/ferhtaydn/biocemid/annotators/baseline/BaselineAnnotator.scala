package com.ferhtaydn.biocemid.annotators.baseline

import com.ferhtaydn.biocemid.annotators.{ Annotator, MethodInfo, MethodWeight }

class BaselineAnnotator(val config: BaselineAnnotatorConfig) extends Annotator {

  override def calculateWeight(sentenceTokens: List[String], info: MethodInfo): MethodWeight = {
    val synonymNgrams = searchInSentence(sentenceTokens, info.nameAndSynonyms)
    MethodWeight(info.id, 1d * synonymNgrams.length, synonymNgrams)
  }
}
