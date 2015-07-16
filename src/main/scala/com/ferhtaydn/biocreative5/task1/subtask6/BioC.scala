package com.ferhtaydn.biocreative5.task1.subtask6

import java.io.{ File, FileOutputStream, FileReader, OutputStreamWriter }

import bioc.io.{ BioCDocumentReader, BioCDocumentWriter, BioCFactory }
import bioc.{ BioCPassage, BioCSentence, BioCCollection, BioCDocument }
import com.typesafe.config.ConfigFactory

import scala.collection.JavaConversions._
import scala.xml.XML

object BioC {

  lazy val methodsInfo = {
    ConfigFactory.load("methods.conf").getConfigList("bioc.psimi.methods").map(MethodInfo(_)).toList
  }

  def getFrequencies(wordsFile: String): Seq[(String, Double)] = {
    val groupedWords = IO.read(wordsFile).foldLeft(Map.empty[String, Double]) {
      (m, word) ⇒ m + (word -> (m.getOrElse(word, 0.0) + 1.0))
    }
    groupedWords.toSeq.sortBy(_._2).reverse
  }

  def extractAnnotatedSentences(file: File, method: String): String = {

    val bioCFile = XML.loadFile(file)
    val passages = bioCFile \\ "passage"
    val annotations = passages \\ "annotation"

    val filtered = annotations.filter(annot ⇒
      (annot \ "infon").filter(i ⇒
        (i \\ "@key").text.equals("PSIMI")
      ).text.equals(method)
    )

    filtered.map(m ⇒ (m \\ "text").text).mkString("\n")

  }

  def tfRf(method: String, wordsFile: String): Seq[(String, Double)] = {

    val methodFreqs = BioC.getFrequencies(wordsFile)
    val otherFiles = IO.listOthers(method).map(_.getPath)

    methodFreqs.map {
      case (word, freq) ⇒

        def log2(x: Double) = scala.math.log(x) / scala.math.log(2)

        val otherTotal = otherFiles.map(BioC.getFrequencies(_).find(wf ⇒ word.equals(wf._1)).fold(0.0)(_._2)).sum

        val rf = log2(2.0 + (freq / scala.math.max(1.0, otherTotal)))
        val tfRf = freq * rf

        (word, tfRf)
    }
  }

  def annotate(dir: String): Unit = {

    IO.list(dir, ".xml").foreach { file ⇒

      val fileName = Utils.extractFileName(file.getName, ".xml")
      val out = s"$dir/${fileName}_passages_with_exp_methods_with_before_after.xml"

      val factory: BioCFactory = BioCFactory.newFactory(BioCFactory.WOODSTOX)
      val reader: BioCDocumentReader = factory.createBioCDocumentReader(new FileReader(file))
      val writer: BioCDocumentWriter = factory.createBioCDocumentWriter(
        new OutputStreamWriter(new FileOutputStream(out), "UTF-8")
      )

      val collection: BioCCollection = reader.readCollectionInfo

      val converter: SentenceConverter2 = new SentenceConverter2

      val outCollection: BioCCollection = converter.getCollection(collection)
      outCollection.setKey("sentence.key")
      writer.writeCollectionInfo(outCollection)

      for (document ← reader) {
        val outDocument: BioCDocument = converter.getDocument(document)
        writer.writeDocument(outDocument)
      }

      reader.close()
      writer.close()

    }
  }

  def splitPassageToSentences(bioCPassage: BioCPassage): Seq[BioCSentence] = {

    lazy val sentences: List[String] = Utils.mkSentences(bioCPassage.getText)

    def loop(sentences: List[String], count: Int, acc: Seq[(String, Int, Int)]): Seq[(String, Int, Int)] = {
      sentences match {
        case Nil     ⇒ acc
        case x :: xs ⇒ loop(xs, x.length + count + 1, acc :+ (x.trim, count + (x.length - x.trim.length), x.trim.length))
      }
    }

    loop(sentences, 0, Seq.empty[(String, Int, Int)]).map {
      case (s, o, l) ⇒
        val sentence: BioCSentence = new BioCSentence
        sentence.setOffset(bioCPassage.getOffset + o)
        sentence.setText(s)
        sentence
    }
  }

  def checkPassageType(bioCPassage: BioCPassage): Boolean = {
    bioCPassage.getInfon("type").contains("title") ||
      bioCPassage.getInfon("type").equalsIgnoreCase("table_caption") ||
      bioCPassage.getInfon("type").equalsIgnoreCase("table") ||
      bioCPassage.getInfon("type").equalsIgnoreCase("ref") ||
      bioCPassage.getInfon("type").equalsIgnoreCase("footnote") ||
      bioCPassage.getInfon("type").equalsIgnoreCase("front")
  }
}
