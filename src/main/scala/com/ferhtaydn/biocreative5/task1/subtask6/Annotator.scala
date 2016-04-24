package com.ferhtaydn.biocreative5.task1.subtask6

import bioc.util.CopyConverter
import bioc.{ BioCAnnotation, BioCLocation, BioCPassage, BioCSentence }

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * core annotator
 */
trait Annotator extends CopyConverter {

  var annotationId: Int = 0
  def mainThreshold: Double
  def smallThreshold: Double
  def beforeAfterCount: Int

  def resetAnnotationId(): Unit = annotationId = 0

  def calculateMethodWeights(words: List[String]): List[MethodWeight]

  def searchInSentence(words: List[String], terms: List[String]): List[String] = {
    terms.flatMap { s ⇒
      val size = s.split("\\s").size
      if (size > 1) {
        Utils.mkNgram(words, size).filter(_.equalsIgnoreCase(s))
      } else {
        words.filter(_.equalsIgnoreCase(s))
      }
    }
  }

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
    setWeights(sentence, calculateMethodWeights(Utils.tokenize(sentence.getText)))
  }

  private def setWeights(sentence: BioCSentence, methodWeights: List[MethodWeight]): BioCSentence = {

    methodWeights.partition(_.weight >= mainThreshold) match {
      case (up, down) ⇒

        if (up.nonEmpty) {

          up.groupBy(_.weight).toSeq.sortBy(_._1).reverse.head._2.foreach {
            case mw ⇒
              val annotationInfons = Map("type" → "ExperimentalMethod", "PSIMI" → mw.id)
              val out: BioCAnnotation = new BioCAnnotation
              out.setInfons(annotationInfons)
              out.setText(sentence.getText)
              out.setLocation(sentence.getOffset, sentence.getText.length)
              sentence.addAnnotation(out)
          }

          sentence

        } else {
          import MethodWeight.toInfons
          sentence.setInfons(mapAsJavaMap(down))
          sentence
        }
    }
  }

  private def annotatePreviousAndNextSentences(annotatedSentences: mutable.MutableList[BioCSentence]): mutable.MutableList[BioCSentence] = {

    annotatedSentences.zipWithIndex.foreach {

      case (sentence, index) ⇒

        if (sentence.getAnnotations.nonEmpty && sentence.getInfons.isEmpty) {

          ((index - beforeAfterCount) to (index + beforeAfterCount)).foreach { i ⇒
            annotatedSentences.get(i).fold() { sent ⇒
              if (sent.getAnnotations.isEmpty && sent.getInfons.nonEmpty) {
                annotateInfon(sentence, sent) match {
                  case Some(annotation) ⇒ sent.addAnnotation(annotation); annotatedSentences.update(i, sent);
                  case None             ⇒ // do nothing
                }
              }
            }
          }
        }
    }

    annotatedSentences

  }

  private def annotateInfon(sentence: BioCSentence, targetSentence: BioCSentence): Option[BioCAnnotation] = {

    val sentenceAnnotation = sentence.getAnnotations.head.getInfon("PSIMI")

    import MethodWeight.fromInfons
    val targetSentenceInfon: List[MethodWeight] = targetSentence.getInfons.toMap

    targetSentenceInfon.find(mw ⇒ mw.id.equals(sentenceAnnotation) && mw.weight >= smallThreshold).map { mw ⇒
      val annotationInfons = Map("type" → "ExperimentalMethod", "PSIMI" → mw.id)
      val out: BioCAnnotation = new BioCAnnotation
      out.setInfons(annotationInfons)
      out.setText(targetSentence.getText)
      out.setLocation(targetSentence.getOffset, targetSentence.getText.length)
      out
    }
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
