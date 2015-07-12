package com.ferhtaydn.biocreative5.task1.subtask6

import java.io.{File, FileOutputStream, FileReader, OutputStreamWriter}

import bioc.io.{BioCDocumentReader, BioCDocumentWriter, BioCFactory}
import bioc.{BioCCollection, BioCDocument}
import com.typesafe.config.ConfigFactory

import scala.collection.JavaConversions._
import scala.xml.XML

object BioC {

  def methodsInfo = ConfigFactory.load().getConfigList("bioc.psimi.methods").map(MethodInfo(_)).toList

  def getFrequencies(wordsFile: String): Seq[(String, Double)] = {
    val groupedWords = io.read(wordsFile).foldLeft(Map.empty[String, Double]) {
      (m, word) => m + (word -> (m.getOrElse(word, 0.0) + 1.0))
    }
    groupedWords.toSeq.sortBy(_._2).reverse
  }

  def extractAnnotatedSentences(file: File, method: String): String = {

    val bioCFile = XML.loadFile(file)
    val passages = bioCFile \\ "passage"
    val annotations = passages \\ "annotation"

    val filtered = annotations.filter(annot =>
      (annot \ "infon").filter(i =>
        (i \\ "@key").text.equals("PSIMI")
      ).text.equals(method)
    )

    filtered.map(m => (m \\ "text").text).mkString("\n")

  }

  def tfRf(method: String, wordsFile: String): Seq[(String, Double)] = {

    val methodFreqs = BioC.getFrequencies(wordsFile)
    val otherFiles = io.listOthers(method).map(f => f.getPath)

    methodFreqs.map { case (word, freq) =>

      def log2(x: Double) = scala.math.log(x) / scala.math.log(2)

      val otherTotal = otherFiles.map(BioC.getFrequencies(_).find(wf => word.equals(wf._1)).fold(0.0)(_._2)).sum

      val rf = log2(2.0 + (freq / scala.math.max(1.0, otherTotal)))
      val tfRf = freq * rf

      (word, tfRf)
    }
  }

  def annotate(dir: String) = {

    io.list(dir, ".xml").foreach { file =>

      val fileName = utils.extractFileName(file.getName, ".xml")
      val out = s"$dir/${fileName}_passages_with_exp_methods.xml"

      val factory: BioCFactory = BioCFactory.newFactory(BioCFactory.WOODSTOX)
      val reader: BioCDocumentReader = factory.createBioCDocumentReader(new FileReader(file))
      val writer: BioCDocumentWriter = factory.createBioCDocumentWriter(
        new OutputStreamWriter(new FileOutputStream(out), "UTF-8")
      )

      val collection: BioCCollection = reader.readCollectionInfo

      val converter: SentenceConverter = new SentenceConverter

      val outCollection: BioCCollection = converter.getCollection(collection)
      outCollection.setKey("sentence.key")
      writer.writeCollectionInfo(outCollection)

      for (document <- reader) {
        val outDocument: BioCDocument = converter.getDocument(document)
        writer.writeDocument(outDocument)
      }

      reader.close()
      writer.close()

    }
  }
}
