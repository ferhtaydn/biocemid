package com.ferhtaydn.biocemid.annotators.word2vec

import com.ferhtaydn.biocemid._
import com.ferhtaydn.biocemid.annotators.{ Annotator, MethodInfo, MethodWeight }

/**
 * This class is used for to look for the previous and next sentences of the annotated sentence.
 * This converter uses also the word2vecs of the methods.
 */
class Word2vecAnnotator(val config: Word2vecAnnotatorConfig) extends Annotator {

  override def calculateMethodWeights(words: List[String]): List[MethodWeight] = {

    methodsInfo.map {

      case info @ MethodInfo(id, name, ss, rs, es, definition, hierarchies) ⇒

        val word2vecs = getWord2vecs(id)

        val synonymNgram = searchInSentence(words, info.nameAndSynonyms)
        val matchingVectors = searchInSentence(words, word2vecs.map(_.phrase).toList)
        val word2vecResults = word2vecs.filter { case Word2vecItem(p, s) ⇒ matchingVectors.contains(p) }

        val n = if (synonymNgram.nonEmpty) 1d else 0d
        val w = if (word2vecResults.nonEmpty) word2vecResults.map(_.score).sum else 0d

        MethodWeight(id, n + w)

    }.filter(_.weight > 0.0).sortWith(_.weight > _.weight)

  }

  private[this] def getWord2vecs(id: String): Seq[Word2vecItem] = {
    list(s"${config.w2vDir}/$id", config.suffix).headOption match {
      case None       ⇒ Seq.empty[Word2vecItem]
      case Some(file) ⇒ read(file).map(line ⇒ Word2vecItem.spacedPhrases(line))
    }
  }
}
