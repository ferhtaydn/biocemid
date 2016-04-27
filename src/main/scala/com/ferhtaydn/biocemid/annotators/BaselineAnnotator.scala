package com.ferhtaydn.biocemid.annotators

import com.ferhtaydn.biocemid.bioc.{ BioC, MethodInfo, MethodWeight }

/**
 * This class is used for to look for the previous and next sentences of the annotated sentence.
 */
class BaselineAnnotator(val annotatorConfig: BaselineAnnotatorConfig) extends Annotator {

  override def calculateMethodWeights(words: List[String]): List[MethodWeight] = {

    BioC.methodsInfo.map {

      case info @ MethodInfo(id, name, ss, rs, es, definition, hierarchies) ⇒

        val synonymNgrams = searchInSentence(words, info.nameAndSynonyms)

        MethodWeight(id, 0.5 * synonymNgrams.size)

    }.filter(_.weight > 0.0).sortWith(_.weight > _.weight)
  }

}
