package com.ferhtaydn.biocreative5.task1.subtask6

import bioc.util.CopyConverter
import bioc.{BioCAnnotation, BioCLocation, BioCPassage, BioCSentence}

import scala.collection.JavaConversions._

class SentenceConverter extends CopyConverter {

  private var annotationId: Int = -1

  override def getPassage(in: BioCPassage): BioCPassage = {

    def annotateSentence(sentence: BioCSentence): Option[BioCAnnotation] = {

      def psiMiDeciderOnVocabularies(words: List[String]): Option[String] = {

        val result = BioC.methodsInfo.map { case MethodInfo(method, ss, rs, es) =>

          val synonymNgram = ss.flatMap { s =>
            val size = s.split("\\s").size
            if (size > 1) {
              utils.mkNgram(words, size).filter(a => a.equalsIgnoreCase(s))
            } else {
              words.filter(w => w.equalsIgnoreCase(s))
            }
          }

          val foundedWords = synonymNgram.flatMap(s => s.split("\\s"))

          val related = words.distinct.diff(foundedWords).flatMap { w =>
            rs.filter(r => r.equalsIgnoreCase(w))
          }

          val extra = words.distinct.diff(foundedWords).flatMap { w =>
            es.filter(e => e.equalsIgnoreCase(w))
          }

          (method, (0.5 * synonymNgram.size) + (0.25 * related.size) + (0.125 * extra.size))

        }.toSeq.sortBy(_._2)

        result.last match {
          case (m, c) =>
            if (c >= 0.5) Some(m) else None
        }
      }

      psiMiDeciderOnVocabularies(utils.tokenize(sentence.getText)) match {
        case None => None
        case Some(psimiInfon) =>

          val annotationInfons = Map("type" -> "ExperimentalMethod", "PSIMI" -> psimiInfon)
          val out: BioCAnnotation = new BioCAnnotation
          out.setInfons(annotationInfons)
          out.setText(sentence.getText)
          annotationId += 1
          out.setID(annotationId.toString)
          out.setLocation(sentence.getOffset, sentence.getText.length)
          Option(out)
      }
    }

    def sentenceWithOffset(sentences: List[String]): Seq[(String, Int, Int)] = {

      def loop(sentences: List[String], count: Int, acc: Seq[(String, Int, Int)]): Seq[(String, Int, Int)] = {
        sentences match {
          case Nil => acc
          case x :: xs => loop(xs, x.length + count + 1, acc :+(x, count, x.length))
        }
      }

      loop(sentences, 0, Seq.empty[(String, Int, Int)])
    }

    val out: BioCPassage = new BioCPassage
    out.setOffset(in.getOffset)
    out.setInfons(in.getInfons)
    out.setText(in.getText)

    if (!in.getInfons.get("type").contains("title")) {

      val sentences: List[String] = utils.mkSentenceList(in.getText)

      sentenceWithOffset(sentences).foreach { case (s, o, l) =>

        val sentence: BioCSentence = new BioCSentence
        sentence.setOffset(in.getOffset + o)
        sentence.setText(s)

        annotateSentence(sentence) match {
          case None =>
          case Some(annotation) =>
            out.addAnnotation(annotation)
        }
      }
    }

    //remove only this line, if you do not want to concat sentences.
    if (out.getAnnotations.size() > 1)
      out.setAnnotations(concatSuccessiveSameAnnotations(out.getAnnotations.toList))
    //remove only this line, if you do not want to concat sentences.

    out
  }

  private def concatSuccessiveSameAnnotations(annotations: List[BioCAnnotation]): List[BioCAnnotation] = {

    def arrangeAnnotationIds(annotations: List[BioCAnnotation]): List[BioCAnnotation] = {

      def loop(annots: List[BioCAnnotation], acc: List[BioCAnnotation], id: Int): List[BioCAnnotation] = annots match {
        case Nil => acc
        case x :: xs =>
          x.setID(id.toString)
          loop(xs, acc :+ x, id + 1)
      }

      val initialId = annotations.head.getID.toInt
      loop(annotations, List[BioCAnnotation](), initialId)
    }

    // a then b
    def successive(a: BioCAnnotation, b: BioCAnnotation): Boolean = {

      val endOfA = locationOf(a).getOffset + locationOf(a).getLength + 1
      val result = a.getInfon("PSIMI").equals(b.getInfon("PSIMI")) && endOfA == locationOf(b).getOffset

      result
    }

    def locationOf(a: BioCAnnotation): BioCLocation = a.getLocations.get(0)

    // a concat b
    def concatAnnotations(a: BioCAnnotation, b: BioCAnnotation): BioCAnnotation = {
      annotationId -= 1
      val out = new BioCAnnotation
      out.setID(a.getID)
      out.setInfons(a.getInfons)
      out.setText(a.getText + " " + b.getText)
      out.setLocation(locationOf(a).getOffset, locationOf(a).getLength + 1 + locationOf(b).getLength)
      out
    }

    def loop(annots: List[BioCAnnotation], acc: List[BioCAnnotation]): List[BioCAnnotation] = annots match {
      case Nil => acc
      case x :: xs if xs.isEmpty => loop(xs, acc :+ x)
      case x :: xs =>
        val y = xs.head
        if (successive(x, y)) {
          val newAnnot = concatAnnotations(x, y)
          loop(newAnnot :: xs.tail, acc)
        } else loop(xs, acc :+ x)
    }

    val result = loop(annotations, List[BioCAnnotation]())
    if (result.size > 1) arrangeAnnotationIds(result) else result

  }

}
