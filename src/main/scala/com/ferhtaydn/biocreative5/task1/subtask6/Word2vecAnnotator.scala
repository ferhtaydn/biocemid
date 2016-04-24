package com.ferhtaydn.biocreative5.task1.subtask6

/**
 * This class is used for to look for the previous and next sentences of the annotated sentence.
 * This converter uses also the word2vecs of the methods.
 */
//noinspection ScalaStyle
class Word2vecAnnotator(word2vecsDir: String, suffix: String, val beforeAfterCount: Int,
    val mainThreshold: Double, val smallThreshold: Double) extends Annotator {

  override def calculateMethodWeights(words: List[String]): List[MethodWeight] = {

    BioC.methodsInfo.map {

      case info @ MethodInfo(id, name, ss, rs, es, definition, hierarchies) ⇒

        lazy val word2vecs = getWord2vecs(id)

        val synonymNgram = searchInSentence(words, info.nameAndSynonyms)

        val matchingVectors = searchInSentence(words, word2vecs.map(_._1).toList)

        val word2vecResults = word2vecs.filter { case (p, s) ⇒ matchingVectors.contains(p) }

        val n = if (synonymNgram.nonEmpty) 1d else 0d
        val w = if (word2vecResults.nonEmpty) word2vecResults.map(_._2).sum else 0d

        MethodWeight(id, n + w)

    }.filter(_.weight > 0.0).sortWith(_.weight > _.weight)

  }

  private[this] def getWord2vecs(id: String): Seq[(String, Double)] = {
    IO.list(s"$word2vecsDir/$id", suffix).headOption match {
      case None ⇒ Seq()
      case Some(file) ⇒
        IO.read(file).map { line ⇒
          val scoreString = line.split(",").last
          val word = line.dropRight(scoreString.length + 1)
          val phrase = word.split("_").mkString(" ")
          val score = scoreString.toDouble
          phrase → score
        }
    }
  }
}
