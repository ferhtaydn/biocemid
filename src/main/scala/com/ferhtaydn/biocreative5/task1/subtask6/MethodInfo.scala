package com.ferhtaydn.biocreative5.task1.subtask6

import com.typesafe.config.Config

import scala.collection.JavaConversions._

case class MethodInfo(name: String, synonym: List[String], related: List[String], extra: List[String])

object MethodInfo {

  def apply(config: Config): MethodInfo = {
    val name = config.getString("name")
    val synonym = config.getStringList("synonym").toList
    val related = config.getStringList("related").toList
    val extra = config.getStringList("extra").toList
    new MethodInfo(name, synonym, related, extra)
  }

}

case class MethodWeight(name: String, weight: Double)

object MethodWeight {
  private def toInfon(mw: MethodWeight): (String, String) = mw.name -> mw.weight.toString
  implicit def toInfons(mws: List[MethodWeight]): Map[String, String] = mws.map(toInfon).toMap
  private def fromInfon(infon: (String, String)): MethodWeight = MethodWeight(infon._1, infon._2.toDouble)
  implicit def fromInfons(infons: Map[String, String]): List[MethodWeight] = infons.map(fromInfon).toList
}
