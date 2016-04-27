package com.ferhtaydn.biocemid.annotators

import com.ferhtaydn.biocemid._
import com.ferhtaydn.biocemid.bioc.{ BioC, MethodInfo, MethodWeight }

/**
 * This class is used for to look for the previous and next sentences of the annotated sentence.
 */
class TfrfAndBaselineAnnotator(isTfrf: Boolean, val beforeAfterCount: Int,
    val mainThreshold: Double, val smallThreshold: Double) extends Annotator {

  override def calculateMethodWeights(words: List[String]): List[MethodWeight] = {

    BioC.methodsInfo.map {

      case info @ MethodInfo(id, name, ss, rs, es, definition, hierarchies) ⇒

        val synonymNgrams = searchInSentence(words, info.nameAndSynonyms)

        if (isTfrf) {

          val foundWords = synonymNgrams.flatMap(split(_, spaceRegex))

          val related = words.distinct.diff(foundWords).flatMap(w ⇒ rs.filter(_.equalsIgnoreCase(w)))

          val extra = words.distinct.diff(foundWords).flatMap(w ⇒ es.filter(_.equalsIgnoreCase(w)))

          MethodWeight(id, (0.5 * synonymNgrams.size) + (0.25 * related.size) + (0.125 * extra.size))

        } else {

          MethodWeight(id, 0.5 * synonymNgrams.size)

        }

    }.filter(_.weight > 0.0).sortWith(_.weight > _.weight)

  }

}
