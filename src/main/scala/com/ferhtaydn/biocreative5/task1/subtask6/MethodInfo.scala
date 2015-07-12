package com.ferhtaydn.biocreative5.task1.subtask6

import com.typesafe.config.Config

import scala.collection.JavaConversions._

case class MethodInfo(name: String, synonym: List[String], related: List[String], extra: List[String])

object MethodInfo {

  def apply(config: Config) = {
    val name = config.getString("name")
    val synonym = config.getStringList("synonym").toList
    val related = config.getStringList("related").toList
    val extra = config.getStringList("extra").toList
    new MethodInfo(name, synonym, related, extra)
  }

}
