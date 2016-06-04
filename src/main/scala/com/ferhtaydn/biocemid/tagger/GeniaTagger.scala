package com.ferhtaydn.biocemid.tagger

import scala.collection.JavaConversions._

class GeniaTagger extends GeniaTaggerWrapper {

  def tag(sentenceTokens: Seq[String]): Seq[GeniaToken] = {
    doTagging(sentenceTokens).map(row ⇒ GeniaToken(row))
  }

  def tagNer(sentenceTokens: Seq[String]): Seq[TokenNamedEntity] = {
    tag(sentenceTokens).map(_.tokenNamedEntity)
  }

  def containsProteinInSentence(sentenceTokens: Seq[String], moreThan: Int = 0): Boolean = {
    tagNer(sentenceTokens).count {
      case TokenNamedEntity(token, namedEntity) ⇒ namedEntity.equals("B-protein")
    } > moreThan
  }

  def containsDifferentProteinsInSentence(sentenceTokens: Seq[String], moreThan: Int = 1): Boolean = {
    tagNer(sentenceTokens).filter {
      case TokenNamedEntity(token, namedEntity) ⇒ namedEntity.equals("B-protein")
    }.groupBy(_.token).size > moreThan
  }

  def containsDifferentProteinsInPassage(passageTokens: Seq[Seq[String]], moreThan: Int = 1): Boolean = {
    passageTokens.flatMap {
      case sentenceTokens ⇒
        tagNer(sentenceTokens).filter {
          case TokenNamedEntity(token, namedEntity) ⇒ namedEntity.equals("B-protein")
        }
    }.groupBy(_.token).size > moreThan
  }

  def containsDifferentProteinsInOneOfTheSentences(passageTokens: Seq[Seq[String]], moreThan: Int = 1): Boolean = {
    passageTokens.exists {
      case sentenceTokens ⇒
        tagNer(sentenceTokens).filter {
          case TokenNamedEntity(token, namedEntity) ⇒ namedEntity.equals("B-protein")
        }.groupBy(_.token).size > moreThan
    }
  }

}
