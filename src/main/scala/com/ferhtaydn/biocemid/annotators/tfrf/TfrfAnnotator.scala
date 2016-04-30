package com.ferhtaydn.biocemid.annotators.tfrf

import com.ferhtaydn.biocemid._
import com.ferhtaydn.biocemid.annotators.{ Annotator, MethodWeight }

/**
 * This class is used for to look for the previous and next sentences of the annotated sentence.
 */
class TfrfAnnotator(val config: TfrfAnnotatorConfig) extends Annotator {

  override def calculateMethodWeights(words: List[String]): List[MethodWeight] = {

    methodsInfo.map {
      case info ⇒
        val synonymNgrams = searchInSentence(words, info.nameAndSynonyms)

        val foundWords = synonymNgrams.flatMap(split(_, spaceRegex))
        val related = words.distinct.diff(foundWords).flatMap(w ⇒ info.relatedTerms.filter(_.equalsIgnoreCase(w)))
        val extra = words.distinct.diff(foundWords).flatMap(w ⇒ info.extraTerms.filter(_.equalsIgnoreCase(w)))

        MethodWeight(info.id, (0.5 * synonymNgrams.size) + (0.25 * related.size) + (0.125 * extra.size))

    }.filter(_.weight > 0.0).sortWith(_.weight > _.weight)
  }
}
