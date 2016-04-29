package com.ferhtaydn.biocemid.annotators.word2vec

import com.ferhtaydn.biocemid._

case class Word2vecItem(phrase: String, score: Double)

object Word2vecItem {

  def underscoredPhrases(s: String): Word2vecItem = {
    val cosineString = split(s, commaRegex).last
    val phrase = s.dropRight(cosineString.length + 1)
    val cosine = cosineString.toDouble
    Word2vecItem(phrase, cosine)
  }

  def spacedPhrases(s: String): Word2vecItem = {
    val scoreString = split(s, commaRegex).last
    val word = s.dropRight(scoreString.length + 1)
    val phrase = mkStringAfterSplit(word, underscoreRegex, space)
    val score = scoreString.toDouble
    Word2vecItem(phrase, score)
  }

  def stringifyItems(seq: Seq[Word2vecItem]): String = {
    val sb = new StringBuilder()
    seq.foreach(i ⇒ sb.append(s"${i.phrase},${i.score}\n"))
    sb.toString()
  }

}
