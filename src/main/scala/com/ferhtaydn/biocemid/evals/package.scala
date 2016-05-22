package com.ferhtaydn.biocemid

import java.io.{ File, FileReader }

import bioc.{ BioCAnnotation, BioCCollection, BioCDocument, BioCPassage }
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
    if (a.equalsIgnoreCase(b)) {
      a
    } else if (a.containsSlice(b)) {
      val startIndex = a.indexOfSlice(b)
      a.substring(startIndex, startIndex + b.length)
      //a.drop(a.indexOfSlice(b)).zip(b).takeWhile { case (x, y) ⇒ x.equals(y) }.unzip._1.mkString
    } else if (b.containsSlice(a)) {
      val startIndex = b.indexOfSlice(a)
      //b.drop(b.indexOfSlice(a)).zip(a).takeWhile { case (x, y) ⇒ x.equals(y) }.unzip._1.mkString
      b.substring(startIndex, startIndex + a.length)
    } else {
      val x = longestCommonSubstring(a, b)
      val y = longestCommonSubstring(b, a)
      if (x.length >= y.length) x else y
    }
  }

  def matchesLocations(a: BioCAnnotation, b: BioCAnnotation): Boolean = {
    a.getLocations.headOption.zip(b.getLocations.headOption).headOption match {
      case Some((x, y)) ⇒
        val (sa, ea) = (x.getOffset, x.getOffset + x.getLength)
        val (sb, eb) = (y.getOffset, y.getOffset + y.getLength)
        !(sa >= eb || sb >= ea)
      case None ⇒ false
    }
  }

  //noinspection ScalaStyle
  //https://github.com/vkostyukov/scalacaster/blob/master/src/primitive/Strings.scala
  private def longestCommonSubstring(a: String, b: String): String = {
    def loop(m: Map[(Int, Int), Int], bestIndices: List[Int], i: Int, j: Int): String = {
      if (i > a.length) {
        b.substring(bestIndices(1) - m((bestIndices(0), bestIndices(1))), bestIndices(1))
      } else if (i == 0 || j == 0) {
        loop(m + ((i, j) → 0), bestIndices, if (j == b.length) i + 1 else i, if (j == b.length) 0 else j + 1)
      } else if (a(i - 1) == b(j - 1) && math.max(m((bestIndices(0), bestIndices(1))), m((i - 1, j - 1)) + 1) == (m((i - 1, j - 1)) + 1)) {
        loop(
          m + ((i, j) → (m((i - 1, j - 1)) + 1)),
          List(i, j),
          if (j == b.length) i + 1 else i,
          if (j == b.length) 0 else j + 1
        )
      } else {
        loop(m + ((i, j) → 0), bestIndices, if (j == b.length) i + 1 else i, if (j == b.length) 0 else j + 1)
      }
    }
    loop(Map[(Int, Int), Int](), List(0, 0), 0, 0)
  }

  def calculateFileResults(results: mutable.Map[Rate, Double], fileRate: FileRate): Unit = {

    results.update(FN, results(FN) + fileRate.fn)
    results.update(FP, results(FP) + fileRate.fp)
    results.update(TN, results(TN) + fileRate.tn)
    results.update(TP, results(TP) + fileRate.tp)

    print(fileRate)

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
