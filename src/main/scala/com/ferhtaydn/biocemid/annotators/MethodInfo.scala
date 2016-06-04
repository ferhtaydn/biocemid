package com.ferhtaydn.biocemid.annotators

import com.typesafe.config.Config

import scala.collection.JavaConversions._

import com.ferhtaydn.biocemid._

case class MethodInfo(id: String, name: String, ontologySynonyms: List[String], addedSynonyms: List[String],
    relatedTerms: List[String], extraTerms: List[String], definition: String, isA: List[Hierarchy]) {

  val nameAndSynonyms: List[String] = name :: ontologySynonyms ++ addedSynonyms
  val pureNameAndSynonyms: List[String] = name :: ontologySynonyms

  val nameAndSynonymsWithUnderscore = nameAndSynonyms.map(mkStringAfterSplit(_, spaceRegex, underscore))
  val pureNameAndSynonymsWithUnderscore = pureNameAndSynonyms.map(mkStringAfterSplit(_, spaceRegex, underscore))

}

case class Hierarchy(id: String, name: String)

object MethodInfo {

  def apply(config: Config): MethodInfo = {
    val id = config.getString("id")
    val name = config.getString("name")
    val ontologySynonyms = config.getStringList("ontologySynonyms").toList
    val addedSynonyms = config.getStringList("addedSynonyms").toList
    val relatedTerms = config.getStringList("relatedTerms").toList
    val extraTerms = config.getStringList("extraTerms").toList
    val definition = config.getString("definition")
    val isA = config.getStringList("isA").toList.map { y ⇒
      val x = y.split("!")
      Hierarchy(x.head, x.last)
    }
    new MethodInfo(id, name, ontologySynonyms, addedSynonyms, relatedTerms, extraTerms, definition, isA)
  }

}

case class MethodWeight(id: String, weight: Double, terms: List[String] = Nil)

object MethodWeight {
  private def toInfon(mw: MethodWeight): (String, String) = mw.id → mw.weight.toString
  implicit def toInfons(mws: List[MethodWeight]): Map[String, String] = mws.map(toInfon).toMap
  private def fromInfon(infon: (String, String)): MethodWeight = MethodWeight(infon._1, infon._2.toDouble)
  implicit def fromInfons(infons: Map[String, String]): List[MethodWeight] = infons.map(fromInfon).toList
}
