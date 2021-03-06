package com.ferhtaydn.biocemid.tagger

import scala.collection.JavaConversions._

class GeniaTagger extends GeniaTaggerWrapper {

  def tag(sentenceTokens: Seq[String]): Seq[GeniaToken] = {
    doTagging(sentenceTokens).map(row ⇒ GeniaToken(row))
  }

  def tagNer(sentenceTokens: Seq[String]): Seq[TokenNamedEntity] = {
    tag(sentenceTokens).map(_.tokenNamedEntity)
  }

  def containsDifferentProteinsInPassage(passageTokens: Seq[Seq[String]], moreThan: Int = 0): Boolean = {
    passageTokens.flatMap {
      case sentenceTokens ⇒
        tagNer(sentenceTokens).filter {
          case TokenNamedEntity(token, namedEntity) ⇒ namedEntity.equals("B-protein")
        }
    }.groupBy(_.token).size > moreThan
  }

}
