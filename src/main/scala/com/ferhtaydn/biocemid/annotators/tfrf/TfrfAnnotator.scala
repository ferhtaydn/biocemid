package com.ferhtaydn.biocemid.annotators.tfrf

import com.ferhtaydn.biocemid._
import com.ferhtaydn.biocemid.annotators.{ Annotator, MethodInfo, MethodWeight }

class TfrfAnnotator(val config: TfrfAnnotatorConfig) extends Annotator {

  override def calculateWeight(sentenceTokens: List[String], info: MethodInfo): MethodWeight = {

    val terms = if (config.pureBaseline) info.pureNameAndSynonyms else info.nameAndSynonyms
    val synonymNgrams = searchInSentence(sentenceTokens, terms)

    val foundWords = synonymNgrams.flatMap(split(_, spaceRegex))
    val related = sentenceTokens.distinct.diff(foundWords).flatMap(w ⇒ info.relatedTerms.filter(_.equalsIgnoreCase(w)))
    val extra = sentenceTokens.distinct.diff(foundWords).flatMap(w ⇒ info.extraTerms.filter(_.equalsIgnoreCase(w)))

    val n = 1d * synonymNgrams.length
    val t = (0.5 * related.length) + (0.25 * extra.length)

    MethodWeight(info.id, n + t, synonymNgrams)
  }
}
