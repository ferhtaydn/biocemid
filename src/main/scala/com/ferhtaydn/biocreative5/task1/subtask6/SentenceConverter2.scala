package com.ferhtaydn.biocreative5.task1.subtask6

import bioc.util.CopyConverter
import bioc.{ BioCAnnotation, BioCLocation, BioCPassage, BioCSentence }

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * This class is used for to look for the previous and next sentences of the annotated sentence.
 */
class SentenceConverter2 extends CopyConverter {

  private[this] var annotationId: Int = 0

  override def getPassage(in: BioCPassage): BioCPassage = {

    val out: BioCPassage = new BioCPassage
    out.setOffset(in.getOffset)
    out.setInfons(in.getInfons)
    out.setText(in.getText)

    if (BioC.checkPassageType(in)) {

      // do nothing for these cases.
      out

    } else {

      val annotatedSentences = mutable.MutableList(BioC.splitPassageToSentences(in).map(annotateSentence)).flatten

      out.setAnnotations(
        concatSuccessiveSameAnnotations(
          annotatePreviousAndNextSentences(annotatedSentences).flatMap(_.getAnnotations).toList
        )
      )

      out
    }

  }

  private def annotateSentence(sentence: BioCSentence): BioCSentence = {

    def calculateMethodWeights(words: List[String]): List[MethodWeight] = {

      BioC.methodsInfo.map {

        case info @ MethodInfo(id, name, ss, rs, es, definition, hierarchies) ⇒

          val synonymNgram = info.nameAndSynonyms.flatMap { s ⇒
            val size = s.split("\\s").size
            if (size > 1) {
              Utils.mkNgram(words, size).filter(_.equalsIgnoreCase(s))
            } else {
              words.filter(_.equalsIgnoreCase(s))
            }
          }

          MethodWeight(id, 0.5 * synonymNgram.size)

      }.filter(_.weight > 0.0).sortWith(_.weight > _.weight)

    }

    def setWeights(sentence: BioCSentence, methodWeights: List[MethodWeight]) = methodWeights match {
      case Nil ⇒ sentence
      case mw :: mws ⇒
        if (mw.weight >= 0.5) {
          val annotationInfons = Map("type" -> "ExperimentalMethod", "PSIMI" -> mw.id)
          val out: BioCAnnotation = new BioCAnnotation
          out.setInfons(annotationInfons)
          out.setText(sentence.getText)
          out.setLocation(sentence.getOffset, sentence.getText.length)
          sentence.addAnnotation(out)
          sentence
        } else {
          import MethodWeight.toInfons
          sentence.setInfons(mapAsJavaMap(methodWeights))
          sentence
        }
    }

    setWeights(sentence, calculateMethodWeights(Utils.tokenize(sentence.getText)))

  }

  private def annotatePreviousAndNextSentences(annotatedSentences: mutable.MutableList[BioCSentence]) = {

    annotatedSentences.zipWithIndex.foreach {

      case (sentence, index) ⇒

        if (sentence.getAnnotations.nonEmpty && sentence.getInfons.isEmpty) {

          if (index > 0 && index < annotatedSentences.size - 1) {

            annotateInfon(index - 1)
            annotateInfon(index + 1)

            def annotateInfon(i: Int) = {

              val sentenceAnnotation = sentence.getAnnotations.head.getInfon("PSIMI")

              import MethodWeight.fromInfons
              val targetSentence = annotatedSentences(i)
              val targetSentenceInfon: List[MethodWeight] = targetSentence.getInfons.toMap

              targetSentenceInfon.find(mw ⇒ mw.id.equals(sentenceAnnotation) && mw.weight >= 0.25).fold() { mw ⇒
                val annotationInfons = Map("type" -> "ExperimentalMethod", "PSIMI" -> mw.id)
                val out: BioCAnnotation = new BioCAnnotation
                out.setInfons(annotationInfons)
                out.setText(targetSentence.getText)
                out.setLocation(targetSentence.getOffset, targetSentence.getText.length)
                targetSentence.addAnnotation(out)
                annotatedSentences.updated(i, targetSentence)
              }
            }
          }
        }
    }

    annotatedSentences

  }

  private def concatSuccessiveSameAnnotations(annotations: List[BioCAnnotation]): List[BioCAnnotation] = {

    def arrangeAnnotationIds(annotations: List[BioCAnnotation]): List[BioCAnnotation] = {

      def loop(annots: List[BioCAnnotation], acc: List[BioCAnnotation]): List[BioCAnnotation] = annots match {
        case Nil ⇒ acc
        case x :: xs ⇒
          x.setID(annotationId.toString)
          annotationId += 1
          loop(xs, acc :+ x)
      }

      loop(annotations, List[BioCAnnotation]())
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
      val out = new BioCAnnotation
      out.setInfons(a.getInfons)
      out.setText(a.getText + " " + b.getText)
      out.setLocation(locationOf(a).getOffset, locationOf(a).getLength + 1 + locationOf(b).getLength)
      out
    }

    def loop(annots: List[BioCAnnotation], acc: List[BioCAnnotation]): List[BioCAnnotation] = annots match {
      case Nil                   ⇒ acc
      case x :: xs if xs.isEmpty ⇒ loop(xs, acc :+ x)
      case x :: xs ⇒
        val y = xs.head
        if (successive(x, y)) {
          val newAnnot = concatAnnotations(x, y)
          loop(newAnnot :: xs.tail, acc)
        } else {
          loop(xs, acc :+ x)
        }
    }

    arrangeAnnotationIds(loop(annotations, List[BioCAnnotation]()))

  }
}
