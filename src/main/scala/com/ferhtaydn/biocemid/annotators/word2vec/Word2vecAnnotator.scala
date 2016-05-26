package com.ferhtaydn.biocemid.annotators.word2vec

import com.ferhtaydn.biocemid._
import com.ferhtaydn.biocemid.annotators.{ Annotator, MethodInfo, MethodWeight }

class Word2vecAnnotator(val config: Word2vecAnnotatorConfig) extends Annotator {

  override def calculateWeight(sentenceTokens: List[String], info: MethodInfo): MethodWeight = {

    val word2vecs = getWord2vecs(info.id)

    val synonymNgram = searchInSentence(sentenceTokens, info.nameAndSynonyms)
    val matchingVectors = searchInSentence(sentenceTokens, word2vecs.map(_.phrase).toList)
    val word2vecResults = word2vecs.filter { case Word2vecItem(p, s) ⇒ matchingVectors.contains(p) }

    val n = 1d * synonymNgram.length
    val w = word2vecResults.map(_.score).sum

    MethodWeight(info.id, n + w)
  }

  private[this] def getWord2vecs(id: String): Seq[Word2vecItem] = {
    list(s"${config.w2vDir}/$id", config.suffix).headOption match {
      case None       ⇒ Seq.empty[Word2vecItem]
      case Some(file) ⇒ read(file).map(line ⇒ Word2vecItem.spacedPhrases(line))
    }
  }
}
