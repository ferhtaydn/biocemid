package com.ferhtaydn.biocreative5.task1.subtask6

import java.io.{ File, FileOutputStream, FileReader, OutputStreamWriter }

import bioc.io.{ BioCDocumentReader, BioCDocumentWriter, BioCFactory }
import bioc.util.CopyConverter
import bioc.{ BioCPassage, BioCSentence, BioCCollection, BioCDocument }
import com.typesafe.config.ConfigFactory

import scala.collection.JavaConversions._
import scala.xml.XML

object BioC {

  lazy val methodsInfo = {
    ConfigFactory.load("methods.conf").getConfigList("bioc.psimi.methods").map(MethodInfo(_)).toList
  }

  lazy val methodIds = methodsInfo.map(_.id)

  def calcFrequenciesFromTokensFile(tokensFile: String): Seq[(String, Double)] = {
    val tokenFreqs = IO.read(tokensFile).foldLeft(Map.empty[String, Double]) {
      (m, word) ⇒ m + (word -> (m.getOrElse(word, 0.0) + 1.0))
    }
    tokenFreqs.toSeq.sortBy(_._2).reverse
  }

  def calcTokenFrequencies(tokens: List[String]): Seq[(String, Double)] = {
    val tokenFreqs = tokens.foldLeft(Map.empty[String, Double]) {
      (m, word) ⇒ m + (word -> (m.getOrElse(word, 0.0) + 1.0))
    }
    tokenFreqs.toSeq.sortBy(_._2).reverse
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

  def tfRf(tokenFreqs: Seq[(String, Double)],
    positivePassages: Seq[String],
    negativePassagesFiles: List[File]): Seq[(String, Double)] = {

    val positiveCategory = positivePassages.map(Utils.tokenize(_).toSet).toList

    val negativeCategory = negativePassagesFiles.flatMap { file ⇒

      val passages = IO.read(file)

      val passageTokens = passages.map(Utils.tokenize(_).toSet)

      passageTokens

    }

    tokenFreqs map {

      case (word, freq) ⇒

        def log2(x: Double) = scala.math.log(x) / scala.math.log(2)

        val a = positiveCategory.count(_.contains(word))

        val c = negativeCategory.count(_.contains(word))

        val rf = log2(2.0 + (a / scala.math.max(1.0, c)))
        val tfRf = freq * rf

        (word, tfRf)
    }
  }

  def tfRfOntology(tokenFreqs: Seq[(String, Double)],
    positivePassages: Seq[String],
    negativePassages: Seq[Set[String]]): Seq[(String, Double)] = {

    val positiveCategory = positivePassages.map(Utils.tokenize(_).toSet).toList

    val negativeCategory = negativePassages

    tokenFreqs map {

      case (word, freq) ⇒

        def log2(x: Double) = scala.math.log(x) / scala.math.log(2)

        val a = positiveCategory.count(_.contains(word)) / positiveCategory.length

        val c = negativeCategory.count(_.contains(word)) / negativeCategory.length

        val rf = log2(2.0 + (a / scala.math.max(1.0, c)))
        val tfRf = freq * rf

        (word, tfRf)
    }
  }

  def annotate(converter: CopyConverter, dir: String, InputFileSuffix: String, outputFileSuffix: String): Unit = {

    IO.list(dir, InputFileSuffix).foreach { file ⇒

      val fileName = Utils.extractFileName(file.getName, InputFileSuffix)
      val out = s"$dir/${fileName}_$outputFileSuffix"

      val factory: BioCFactory = BioCFactory.newFactory(BioCFactory.WOODSTOX)
      val reader: BioCDocumentReader = factory.createBioCDocumentReader(new FileReader(file))
      val writer: BioCDocumentWriter = factory.createBioCDocumentWriter(
        new OutputStreamWriter(new FileOutputStream(out), "UTF-8")
      )

      val collection: BioCCollection = reader.readCollectionInfo

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

  def countOfMethods(dir: String, suffix: String): Unit = {

    val methodCounts: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)

    IO.list(dir, suffix).foreach { file ⇒

      val factory: BioCFactory = BioCFactory.newFactory(BioCFactory.WOODSTOX)

      val manuReader: BioCDocumentReader = factory.createBioCDocumentReader(new FileReader(file))

      val manuCollection: BioCCollection = manuReader.readCollectionInfo
      val manuDocument: BioCDocument = manuReader.readDocument()

      val manuPassages = manuDocument.getPassages.filterNot(checkPassageType)

      manuPassages.foreach { pas ⇒

        pas.getAnnotations.toList.groupBy(a ⇒ a.getInfon("PSIMI")).map(x ⇒ x._1 -> x._2.size).foreach {
          case (n, c) ⇒

            methodCounts.update(n, methodCounts(n) + c)
        }

      }
    }

    println(methodCounts)
  }

  def evaluate(manuResultDir: String, algoResultDir: String, fileSuffix: String) = {

    val manuAnnotationFiles = IO.list(manuResultDir, fileSuffix)
    val algorithmAnnotationFiles = IO.list(algoResultDir, fileSuffix)

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

        var falseNegatives: Int = 0
        var falsePositives: Int = 0
        var trueNegatives: Int = 0
        var truePositives: Int = 0

        val algoPassages = algoDocument.getPassages.filterNot(checkPassageType)
        val manuPassages = manuDocument.getPassages.filterNot(checkPassageType)

        manuPassages.zip(algoPassages).foreach {
          case (mp, ap) ⇒

            mp.getAnnotations.foreach { a ⇒

              if (a.getInfon("PSIMI").equals("0006") || a.getInfon("PSIMI").equals("0007")) {
                a.putInfon("PSIMI", "0019")
              }
            }

            if (mp.getAnnotations.isEmpty && ap.getAnnotations.isEmpty) {
              trueNegatives += 1
            } else if (mp.getAnnotations.isEmpty && ap.getAnnotations.nonEmpty) {
              //println("mp empty, ap nonEmpty size: " + ap.getAnnotations.size)
              falsePositives += ap.getAnnotations.size
            } else if (mp.getAnnotations.nonEmpty && ap.getAnnotations.isEmpty) {
              //println("ap empty, mp nonEmpty size: " + mp.getAnnotations.size)
              falseNegatives += mp.getAnnotations.size
            } else if (mp.getAnnotations.size > ap.getAnnotations.size) {

              falseNegatives += (mp.getAnnotations.size - ap.getAnnotations.size)

              //println("mp > ap -> " + mp.getAnnotations.size + " > " + ap.getAnnotations.size)

              ap.getAnnotations.foreach { aa ⇒

                var found: Boolean = false
                val aaSentences = Utils.mkSentences(aa.getText)

                //println("algo annotation: " + aa.getInfon("PSIMI") + " " + aa.getText)
                //println(aaSentences.mkString("\n"))

                mp.getAnnotations.foreach { ma ⇒
                  val maSentences = Utils.mkSentences(ma.getText)

                  //println("manual annotation: " + ma.getInfon("PSIMI") + " " + ma.getText)
                  //println(maSentences.mkString("\n"))

                  if (aa.getInfon("PSIMI").equals(ma.getInfon("PSIMI")) && maSentences.intersect(aaSentences).nonEmpty) {
                    //println("found")
                    found = true
                  }
                }

                if (found) truePositives += 1 else falsePositives += 1
              }

            } else if (mp.getAnnotations.size < ap.getAnnotations.size) {

              //println("mp < ap -> " + mp.getAnnotations.size + " < " + ap.getAnnotations.size)

              falsePositives += (ap.getAnnotations.size - mp.getAnnotations.size)

              mp.getAnnotations.foreach { ma ⇒

                var found: Boolean = false
                val maSentences = Utils.mkSentences(ma.getText)

                //println("manual annotation: " + ma.getInfon("PSIMI") + " " + ma.getText)
                //println(maSentences.mkString("\n"))

                ap.getAnnotations.foreach { aa ⇒
                  val aaSentences = Utils.mkSentences(aa.getText)

                  //println("algo annotation: " + aa.getInfon("PSIMI") + " " + aa.getText)
                  //println(aaSentences.mkString("\n"))

                  if (aa.getInfon("PSIMI").equals(ma.getInfon("PSIMI")) && maSentences.intersect(aaSentences).nonEmpty) {
                    //println("found")
                    found = true
                  }
                }
                if (found) truePositives += 1 else falsePositives += 1
              }

            } else if (mp.getAnnotations.size == ap.getAnnotations.size) {

              //println("mp == ap")

              mp.getAnnotations.foreach { ma ⇒

                var found: Boolean = false
                val maSentences = Utils.mkSentences(ma.getText)

                //println("manual annotation: " + ma.getInfon("PSIMI") + " " + ma.getText)
                //println(maSentences.mkString("\n"))

                ap.getAnnotations.foreach { aa ⇒
                  val aaSentences = Utils.mkSentences(aa.getText)

                  //println("algo annotation: " + aa.getInfon("PSIMI") + " " + aa.getText)
                  //println(aaSentences.mkString("\n"))

                  if (aa.getInfon("PSIMI").equals(ma.getInfon("PSIMI")) && maSentences.intersect(aaSentences).nonEmpty) {
                    //println("found")
                    found = true
                  }
                }
                if (found) truePositives += 1 else falsePositives += 1

              }

            }
        }

        println("falseNegatives: " + falseNegatives)
        println("falsePositives: " + falsePositives)
        println("trueNegatives: " + trueNegatives)
        println("truePositives: " + truePositives)
        println()

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
