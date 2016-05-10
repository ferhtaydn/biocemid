package com.ferhtaydn.biocemid

import java.io.{ File, FileReader }

import bioc.{ BioCCollection, BioCDocument, BioCPassage }
import bioc.io.{ BioCDocumentReader, BioCFactory }

import scala.collection.mutable
import scala.collection.JavaConversions._

package object evals {

  def combineCoImmunoPrecipitations(passage: BioCPassage): Unit = {
    passage.getAnnotations.foreach { a ⇒
      if (a.getInfon(psimi).equals("0006") || a.getInfon(psimi).equals("0007")) a.putInfon(psimi, "0019")
    }
  }

  def getBioCPassages(file: File): List[BioCPassage] = {
    val factory: BioCFactory = BioCFactory.newFactory(BioCFactory.WOODSTOX)
    val reader: BioCDocumentReader = factory.createBioCDocumentReader(new FileReader(file))
    val collection: BioCCollection = reader.readCollectionInfo
    val document: BioCDocument = reader.readDocument()
    document.getPassages.filterNot(_.skip).toList
  }

  def getCommonText(a: String, b: String): String = {
    a.zip(b).takeWhile { case (x, y) ⇒ x.equals(y) }.unzip._1.mkString
  }

  def containsCommonText(a: String, b: String): Boolean = {
    getCommonText(a, b).nonEmpty
  }

  def calculateFileResults(results: mutable.Map[Rate, Double], fileRate: FileRate): Unit = {

    results.update(FN, results(FN) + fileRate.fn)
    results.update(FP, results(FP) + fileRate.fp)
    results.update(TN, results(TN) + fileRate.tn)
    results.update(TP, results(TP) + fileRate.tp)

    println(fileRate)

  }

  def calculateTotalResults(results: Map[Rate, Double]): Unit = {

    val accuracy = (results(TP) + results(TN)) / results.values.toList.sum
    val precision = results(TP) / (results(TP) + results(FP))
    val recall = results(TP) / (results(TP) + results(FN))
    val fScore = (2 * precision * recall) / (precision + recall)

    println(results)
    println(s"accuracy: $accuracy")
    println(s"precision: $precision")
    println(s"recall: $recall")
    println(s"fscore: $fScore")
  }
}
