package com.ferhtaydn.biocemid

import java.io.FileReader

import bioc.io.{ BioCDocumentReader, BioCFactory }
import bioc.{ BioCCollection, BioCDocument, BioCPassage }

import scala.collection.JavaConversions._
import scala.collection.mutable

//noinspection ScalaStyle
object Evaluator {

  def countOfMethods(dir: String, suffix: String): Unit = {

    val methodCountWithinPassages = mutable.Map.empty[String, Int].withDefaultValue(0)
    val methodCountWithinArticles = mutable.Map.empty[String, Set[String]].withDefaultValue(Set.empty[String])

    list(dir, suffix).foreach { file ⇒

      val factory: BioCFactory = BioCFactory.newFactory(BioCFactory.WOODSTOX)
      val manuReader: BioCDocumentReader = factory.createBioCDocumentReader(new FileReader(file))
      val manuCollection: BioCCollection = manuReader.readCollectionInfo
      val manuDocument: BioCDocument = manuReader.readDocument()
      val manuPassages = manuDocument.getPassages.filterNot(_.skip)

      manuPassages.foreach { pas ⇒
        pas.getAnnotations.toList.groupBy(a ⇒ a.getInfon(psimi)).map(x ⇒ x._1 → x._2.size).foreach {
          case (n, c) ⇒
            methodCountWithinPassages.update(n, methodCountWithinPassages(n) + c)
            methodCountWithinArticles.update(n, methodCountWithinArticles(n) + file.getName)
        }
      }
    }

    methodIds.foreach { id ⇒
      Console.println(
        s"""
           |MI:$id
           |In ${methodCountWithinArticles(id).size} articles: ${methodCountWithinArticles(id)}
           |In ${methodCountWithinPassages(id)} passages
         """.stripMargin
      )
    }
  }

  def evaluate(manuResultDir: String, algoResultDir: String, fileSuffix: String): Unit = {

    val manuAnnotationFiles = list(manuResultDir, fileSuffix)
    val algorithmAnnotationFiles = list(algoResultDir, fileSuffix)

    val results = scala.collection.mutable.Map.empty[String, Double].withDefaultValue(0)

    def combineCoImmunoPrecipitations(mp: BioCPassage): Unit = {
      mp.getAnnotations.foreach { a ⇒
        if (a.getInfon(psimi).equals("0006") || a.getInfon(psimi).equals("0007")) {
          a.putInfon(psimi, "0019")
        }
      }
    }

    manuAnnotationFiles.zip(algorithmAnnotationFiles).foreach {
      case (manu, algo) ⇒

        val factory: BioCFactory = BioCFactory.newFactory(BioCFactory.WOODSTOX)
        val manuReader: BioCDocumentReader = factory.createBioCDocumentReader(new FileReader(manu))
        val algoReader: BioCDocumentReader = factory.createBioCDocumentReader(new FileReader(algo))
        val manuCollection: BioCCollection = manuReader.readCollectionInfo
        val manuDocument: BioCDocument = manuReader.readDocument()
        val algoCollection: BioCCollection = algoReader.readCollectionInfo
        val algoDocument: BioCDocument = algoReader.readDocument()

        println(s"document id: ${manuDocument.getID}")

        var falseNegatives: Double = 0d
        var falsePositives: Double = 0d
        var trueNegatives: Double = 0d
        var truePositives: Double = 0d

        val algoPassages = algoDocument.getPassages.filterNot(_.skip)
        val manuPassages = manuDocument.getPassages.filterNot(_.skip)

        manuPassages.zip(algoPassages).foreach {
          case (mp, ap) ⇒

            combineCoImmunoPrecipitations(mp)

            if (mp.getAnnotations.isEmpty && ap.getAnnotations.isEmpty) {
              trueNegatives += 1
            } else if (mp.getAnnotations.isEmpty && ap.getAnnotations.nonEmpty) {
              falsePositives += ap.getAnnotations.size
            } else if (mp.getAnnotations.nonEmpty && ap.getAnnotations.isEmpty) {
              falseNegatives += mp.getAnnotations.size
            } else if (mp.getAnnotations.size > ap.getAnnotations.size) {
              falseNegatives += (mp.getAnnotations.size - ap.getAnnotations.size)

              ap.getAnnotations.foreach { aa ⇒
                var found: Boolean = false
                val aaSentences = mkSentences(aa.getText)

                mp.getAnnotations.foreach { ma ⇒
                  val maSentences = mkSentences(ma.getText)
                  if (aa.getInfon(psimi).equals(ma.getInfon(psimi)) && maSentences.intersect(aaSentences).nonEmpty) {
                    found = true
                  }
                }

                if (found) truePositives += 1 else falsePositives += 1
              }

            } else if (mp.getAnnotations.size < ap.getAnnotations.size) {
              falsePositives += (ap.getAnnotations.size - mp.getAnnotations.size)

              mp.getAnnotations.foreach { ma ⇒

                var found: Boolean = false
                val maSentences = mkSentences(ma.getText)

                ap.getAnnotations.foreach { aa ⇒
                  val aaSentences = mkSentences(aa.getText)

                  if (aa.getInfon(psimi).equals(ma.getInfon(psimi)) && maSentences.intersect(aaSentences).nonEmpty) {
                    found = true
                  }
                }
                if (found) truePositives += 1 else falsePositives += 1
              }

            } else if (mp.getAnnotations.size == ap.getAnnotations.size) {

              mp.getAnnotations.foreach { ma ⇒

                var found: Boolean = false
                val maSentences = mkSentences(ma.getText)

                ap.getAnnotations.foreach { aa ⇒
                  val aaSentences = mkSentences(aa.getText)

                  if (aa.getInfon(psimi).equals(ma.getInfon(psimi)) && maSentences.intersect(aaSentences).nonEmpty) {
                    found = true
                  }
                }
                if (found) truePositives += 1 else falsePositives += 1
              }
            }
        }

        results.update("FN", results("FN") + falseNegatives)
        results.update("FP", results("FP") + falsePositives)
        results.update("TN", results("TN") + trueNegatives)
        results.update("TP", results("TP") + truePositives)

        println("falseNegatives: " + falseNegatives)
        println("falsePositives: " + falsePositives)
        println("trueNegatives: " + trueNegatives)
        println("truePositives: " + truePositives)
        println()

    }

    calculateResults(results)

  }

  def calculateResults(results: mutable.Map[String, Double]): Unit = {
    println(results)
    val accuracy = (results("TP") + results("TN")) / results.values.toList.sum
    val precision = results("TP") / (results("TP") + results("FP"))
    val recall = results("TP") / (results("TP") + results("FN"))
    val fscore = (2 * precision * recall) / (precision + recall)

    println(s"accuracy: $accuracy")
    println(s"precision: $precision")
    println(s"recall: $recall")
    println(s"fscore: $fscore")
  }
}
