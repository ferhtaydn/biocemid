package com.ferhtaydn.biocreative5.task1.subtask6

import java.util.Properties

import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation
import edu.stanford.nlp.pipeline.{ Annotation, StanfordCoreNLP }

import scala.collection.JavaConversions._

object Utils {

  def extractFileName(fileName: String, suffix: String): String = fileName.split(suffix).head

  def stringifyTuple2Sequence(seq: Seq[(String, Double)]): String = {
    val sb = new StringBuilder()
    seq.foreach(a ⇒ sb.append(s"${a._1} ${a._2}\n"))
    sb.toString()
  }

  def mkSentence(text: String): String = text.split("\\.\\s").mkString(".\n")

  def mkSentences(text: String): List[String] = text.split("\\.\\s").toList match {
    case Nil                         ⇒ Nil
    case lst @ x :: xs if xs.isEmpty ⇒ lst
    case lst @ x :: xs ⇒
      lst.map { s ⇒
        s.lastOption.fold(s) { l ⇒
          if (!l.equals('.')) s.concat(".") else s
        }
      }
  }

  def mkNgram(elems: List[String], size: Int): List[String] = elems.sliding(size).map(_.mkString(" ")).toList

  lazy val initTokenization: StanfordCoreNLP = {
    val properties = new Properties()
    properties.setProperty("annotators", "tokenize")
    val coreNLP = new StanfordCoreNLP(properties)
    coreNLP
  }

  lazy val stopwords = IO.read("stopwords.txt").toList

  def tokenize(sentence: String): List[String] = {

    val annotation = new Annotation(sentence.toLowerCase)
    initTokenization.annotate(annotation)
    annotation.get(classOf[TokensAnnotation]).map(_.value).toList.filterNot { a ⇒
      a.length < 2 || stopwords.contains(a) || a.isFloat || a.isDigit
    }
  }

  implicit class StringUtils(val s: String) extends AnyVal {
    def isFloat: Boolean = s.matches("[+-]?\\d+.?\\d+")
    def isDigit: Boolean = s.matches("[+-]?\\d+")
  }
}
