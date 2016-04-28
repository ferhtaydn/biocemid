package com.ferhtaydn.biocemid.annotators.baseline

import com.ferhtaydn.biocemid.annotators.{ Annotator, MethodInfo, MethodWeight }
import com.ferhtaydn.biocemid._

/**
 * This class is used for to look for the previous and next sentences of the annotated sentence.
 */
class BaselineAnnotator(val config: BaselineAnnotatorConfig) extends Annotator {

  override def calculateMethodWeights(words: List[String]): List[MethodWeight] = {

    methodsInfo.map {

      case info @ MethodInfo(id, name, ss, rs, es, definition, hierarchies) ⇒

        val synonymNgrams = searchInSentence(words, info.nameAndSynonyms)

        MethodWeight(id, 0.5 * synonymNgrams.size)

    }.filter(_.weight > 0.0).sortWith(_.weight > _.weight)
  }

}
