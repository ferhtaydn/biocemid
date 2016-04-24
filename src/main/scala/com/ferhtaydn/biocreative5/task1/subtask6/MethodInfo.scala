package com.ferhtaydn.biocreative5.task1.subtask6

import com.typesafe.config.Config

import scala.collection.JavaConversions._

case class MethodInfo(id: String, name: String, synonym: List[String], related: List[String], extra: List[String],
    definition: String, isA: List[Hierarchy]) {

  def nameAndSynonyms: List[String] = if (!synonym.contains(name)) name :: synonym else name :: (synonym diff List(name))

}

case class Hierarchy(id: String, name: String)

object MethodInfo {

  def apply(config: Config): MethodInfo = {
    val id = config.getString("id")
    val name = config.getString("name")
    val synonym = config.getStringList("synonym").toList
    val related = config.getStringList("related").toList
    val extra = config.getStringList("extra").toList
    val definition = config.getString("definition")
    val isA = config.getStringList("isA").toList.map { y ⇒
      val x = y.split("!")
      Hierarchy(x.head, x.last)
    }
    new MethodInfo(id, name, synonym, related, extra, definition, isA)
  }

}

case class MethodWeight(id: String, weight: Double)

object MethodWeight {
  private def toInfon(mw: MethodWeight): (String, String) = mw.id → mw.weight.toString
  implicit def toInfons(mws: List[MethodWeight]): Map[String, String] = mws.map(toInfon).toMap
  private def fromInfon(infon: (String, String)): MethodWeight = MethodWeight(infon._1, infon._2.toDouble)
  implicit def fromInfons(infons: Map[String, String]): List[MethodWeight] = infons.map(fromInfon).toList
}
