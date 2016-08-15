package com.ferhtaydn

import java.util.Properties
import java.io.{ File, FileWriter }
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, StandardOpenOption }

import bioc.BioCPassage
import com.ferhtaydn.biocemid.annotators.MethodInfo
import com.typesafe.config.ConfigFactory
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation
import edu.stanford.nlp.pipeline.{ Annotation, StanfordCoreNLP }

import scala.collection.JavaConversions._
import scala.util.Try

package object biocemid {

  val psimi: String = "PSIMI"

  lazy val methodsInfo: List[MethodInfo] = {
    ConfigFactory.load("methods.conf").getConfigList("psimi.methods").map(MethodInfo(_)).toList
  }

  lazy val inoTerms: Seq[String] = {
    read("files/ino/literature_mining_keywords_related_to_proteins.txt")
      .filterNot(line ⇒ line.contains(":") || line.isEmpty)
      .flatMap(line ⇒ split(line, commaRegex))
  }

  lazy val inoInteractionTerms: Seq[String] = {
    read("files/ino/interaction_keywords.txt")
      .filterNot(line ⇒ line.contains(":") || line.isEmpty)
      .flatMap(line ⇒ split(line, commaRegex))
  }

  lazy val methodIds: List[String] = methodsInfo.map(_.id)

  // ...regex to split, normals to mkString
  val period: String = "."
  val periodChar: Char = '.'
  val periodRegex: String = "\\."
  val comma: String = ","
  val commaRegex: String = "\\,"
  val periodNewline: String = ".\n"
  val newline: String = "\n"
  val space: String = " "
  val spaceRegex: String = "\\s"
  val periodSpace: String = ". "
  val periodSpaceRegex: String = periodRegex + spaceRegex
  val underscore: String = "_"
  val underscoreRegex: String = "\\_"

  def split(s: String, regex: String): List[String] = s.split(regex).map(_.trim).toList

  def mkStringAfterSplit(s: String, regex: String, sep: String): String = split(s, regex).mkString(sep)

  def extractFileName(fileName: String, suffix: String): String = split(fileName, suffix).head

  def stringifyTuples[A, B](seq: Seq[(A, B)]): String = {
    val sb = new StringBuilder()
    seq.foreach(a ⇒ sb.append(s"${a._1},${a._2}\n"))
    sb.toString()
  }

  def mkSentence(text: String): String = mkStringAfterSplit(text, periodSpaceRegex, periodNewline)

  def mkSentences(text: String): List[String] = text.split(periodSpaceRegex).toList match {
    case Nil                         ⇒ Nil
    case lst @ x :: xs if xs.isEmpty ⇒ lst
    case lst @ x :: xs ⇒
      lst.map { s ⇒
        s.lastOption.fold(s) { l ⇒
          if (!l.equals(periodChar)) s.concat(period) else s
        }
      }
  }

  def mkNgram(elems: List[String], size: Int): List[String] = elems.sliding(size).map(_.mkString(space)).toList

  lazy val stopwords = read("files/stopwords.txt")

  lazy val initTokenization: StanfordCoreNLP = {
    val properties = new Properties()
    properties.setProperty("annotators", "tokenize")
    val coreNLP = new StanfordCoreNLP(properties)
    coreNLP
  }

  def tokenize(sentence: String): List[String] = {
    val annotation = new Annotation(sentence.toLowerCase)
    initTokenization.annotate(annotation)
    annotation.get(classOf[TokensAnnotation]).map(_.value).toList.filterNot { a ⇒
      a.length < 2 || stopwords.contains(a) || a.isFloat || a.isDigit
    }
  }

  def tokenizeForGeniaTagger(sentence: String): Seq[String] = {
    val annotation = new Annotation(sentence)
    initTokenization.annotate(annotation)
    annotation.get(classOf[TokensAnnotation]).map(_.value)
  }

  implicit class StringUtils(val s: String) extends AnyVal {
    def isFloat: Boolean = s.matches("[+-]?\\d+.?\\d+")
    def isDigit: Boolean = s.matches("[+-]?\\d+")
  }

  implicit class BioCPassageOps(val passage: BioCPassage) extends AnyVal {

    def skip: Boolean = {

      val infonType = passage.getInfon("type")

      split(passage.getText, spaceRegex).length < 5 ||
        infonType.contains("title") ||
        infonType.equalsIgnoreCase("table_caption") ||
        infonType.equalsIgnoreCase("table") ||
        infonType.equalsIgnoreCase("ref") ||
        infonType.equalsIgnoreCase("footnote") ||
        infonType.equalsIgnoreCase("front")
    }
  }

  // gold_set_13, gold_set_17 and gold_set_30 files contains the articles from $manualAnnotationStatistics
  val goldResultDirectory = "files/gold_set_13"
  val publishedDataSet = "files/published_dataset"

  val word2vecResultDirectory = "files/results/word2vec/pure-baseline/config_3b_13_articles_genia0_ino1"
  val tfrfResultDirectory = "files/results/tfrf/pure-baseline/manual/config_1_17_articles"
  val pureBaselineResultDirectory = "files/results/pure-baseline/pure-baseline_17_articles_genia0_ino1"

  val rawDirectory = "files/manual_annotation_raw_13"

  val oaWord2vecsDirectory = "files/oa_word2vecs"
  val oaWord2vecsPureBaselineDirectory = "files/oa_word2vecs_pure_baseline"

  val xmlSuffix = ".xml"
  val txtSuffix = ".txt"
  val word2vecResultFileSuffix = "result.txt"
  val word2vecResultRawFileSuffix = "result_raw.txt"
  val word2vecResultDedupeFileSuffix = "result_dedupe.txt"
  val word2vecResultDedupeEnhancedFileSuffix = "result_dedupe_enhanced.txt"
  val baselineAnnotatedSuffix = "baseline.xml"
  val tfrfAnnotatedSuffix = "tfrf.xml"
  val word2vecAnnotatedSuffix = "word2vec.xml"

  def write(path: String, txt: String): Unit = {
    Files.write(Paths.get(path), txt.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE)
  }

  def append(path: String, txt: String): Unit = {
    val fw = new FileWriter(path, true)
    fw.write(txt)
    fw.close()
  }

  def list(dirName: String, suffix: String): List[File] = {
    val dir = new File(dirName)
    if (dir.exists()) {
      dir.listFiles.filter(_.isFile).filter(_.getName.endsWith(suffix)).toList
    } else {
      Nil
    }
  }

  def listOthers(method: String, suffix: String): List[File] = {
    new File(period).listFiles.filter(_.isFile).filter { f ⇒
      f.getName.contains(suffix) && !f.getName.contains(method)
    }.toList
  }

  def remove(file: String): Boolean = Files.deleteIfExists(Paths.get(file))

  def read(path: String): Seq[String] = Try(scala.io.Source.fromFile(path)).toOption match {
    case Some(bufferedSource) ⇒ bufferedSource.getLines().toSeq
    case None                 ⇒ Seq.empty[String]
  }

  def read(file: File): Seq[String] = Try(scala.io.Source.fromFile(file)).toOption match {
    case Some(bufferedSource) ⇒ bufferedSource.getLines().toSeq
    case None                 ⇒ Seq.empty[String]
  }

}
