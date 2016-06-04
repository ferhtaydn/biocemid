package com.ferhtaydn.biocemid.tagger

case class GeniaToken(token: String, baseForm: String, pos: String, chunk: String, namedEntity: String) {
  val tokenNamedEntity = TokenNamedEntity(token, namedEntity)
}

object GeniaToken {
  def apply(fields: Array[String]): GeniaToken = {
    val Array(token, baseForm, pos, chunk, namedEntity) = fields
    GeniaToken(token, baseForm, pos, chunk, namedEntity)
  }
}

case class TokenNamedEntity(token: String, namedEntity: String)
