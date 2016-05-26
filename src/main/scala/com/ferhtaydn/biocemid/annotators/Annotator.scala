package com.ferhtaydn.biocemid.annotators

import bioc.{ BioCAnnotation, BioCLocation, BioCPassage, BioCSentence }
import bioc.util.CopyConverter

import scala.collection.mutable

import com.ferhtaydn.biocemid._

import scala.collection.JavaConversions._

/**
 * core annotator
 */
abstract class Annotator extends CopyConverter {

  private var annotationId: Int = 0

  val config: AnnotatorConfig

  def resetAnnotationId(): Unit = annotationId = 0

  def calculateWeight(sentenceTokens: List[String], info: MethodInfo): MethodWeight

  def calculateMethodWeights(sentenceTokens: List[String]): List[MethodWeight] = {
    methodsInfo.map {
      case info ⇒
        calculateWeight(sentenceTokens, info)
    }.filter(_.weight > 0.0).sortWith(_.weight > _.weight)
  }

  def searchInSentence(sentenceTokens: List[String], terms: List[String]): List[String] = {
    terms.flatMap { s ⇒
      val size = split(s, spaceRegex).size
      if (size > 1) {
        mkNgram(sentenceTokens, size).filter(_.equalsIgnoreCase(s))
      } else {
        sentenceTokens.filter(_.equalsIgnoreCase(s))
      }
    }
  }

  override def getPassage(in: BioCPassage): BioCPassage = {

    val out: BioCPassage = new BioCPassage
    out.setOffset(in.getOffset)
    out.setInfons(in.getInfons)
    out.setText(in.getText)

    if (in.skip) {
      out
    } else {

      val annotatedSentences = mutable.MutableList(splitPassageToSentences(in).map(annotateSentence)).flatten

      out.setAnnotations(
        concatSuccessiveSameAnnotations(
          annotatePreviousAndNextSentences(annotatedSentences).flatMap(_.getAnnotations).toList
        )
      )
      out
    }
  }

  private def splitPassageToSentences(bioCPassage: BioCPassage): Seq[BioCSentence] = {

    def go(sentences: List[String], count: Int, acc: Seq[(String, Int, Int)]): Seq[(String, Int, Int)] = {
      sentences match {
        case Nil ⇒ acc
        case x :: xs ⇒
          val t3 = (x.trim, count + (x.length - x.trim.length), x.trim.length)
          go(xs, x.length + count + 1, acc :+ t3)
      }
    }

    go(mkSentences(bioCPassage.getText), 0, Seq.empty[(String, Int, Int)]).map {
      case (s, o, l) ⇒
        val sentence: BioCSentence = new BioCSentence
        sentence.setOffset(bioCPassage.getOffset + o)
        sentence.setText(s)
        sentence
    }
  }

  private def annotateSentence(sentence: BioCSentence): BioCSentence = {
    setWeights(sentence, calculateMethodWeights(tokenize(sentence.getText)))
  }

  private def setWeights(sentence: BioCSentence, methodWeights: List[MethodWeight]): BioCSentence = {

    methodWeights.partition(_.weight >= config.mainThreshold) match {
      case (up, down) ⇒
        if (up.nonEmpty) {
          up.groupBy(_.weight).toSeq.sortBy(_._1).reverse.head._2.foreach {
            case mw ⇒ sentence.addAnnotation(prepareAnnotation(sentence, mw.id))
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

          ((index - config.beforeAfterCount) to (index + config.beforeAfterCount)).foreach { i ⇒
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

    val sentenceAnnotation = sentence.getAnnotations.head.getInfon(psimi)

    import MethodWeight.fromInfons
    val targetSentenceInfon: List[MethodWeight] = targetSentence.getInfons.toMap

    targetSentenceInfon.find(mw ⇒ mw.id.equals(sentenceAnnotation) && mw.weight >= config.smallThreshold).map { mw ⇒
      prepareAnnotation(targetSentence, mw.id)
    }
  }

  private def prepareAnnotation(targetSentence: BioCSentence, methodId: String): BioCAnnotation = {
    val out: BioCAnnotation = new BioCAnnotation
    out.setInfons(Map("type" → "ExperimentalMethod", psimi → methodId))
    out.setText(targetSentence.getText)
    out.setLocation(targetSentence.getOffset, targetSentence.getText.length)
    out
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
      val result = a.getInfon(psimi).equals(b.getInfon(psimi)) && endOfA == locationOf(b).getOffset

      result
    }

    def locationOf(a: BioCAnnotation): BioCLocation = a.getLocations.get(0)

    // a concat b
    def concatAnnotations(a: BioCAnnotation, b: BioCAnnotation): BioCAnnotation = {
      val out = new BioCAnnotation
      out.setInfons(a.getInfons)
      out.setText(a.getText + space + b.getText)
      out.setLocation(locationOf(a).getOffset, locationOf(a).getLength + 1 + locationOf(b).getLength)
      out
    }

    def go(annots: List[BioCAnnotation], acc: List[BioCAnnotation]): List[BioCAnnotation] = annots match {
      case Nil                   ⇒ acc
      case x :: xs if xs.isEmpty ⇒ go(xs, acc :+ x)
      case x :: xs ⇒
        val y = xs.head
        if (successive(x, y)) {
          val newAnnot = concatAnnotations(x, y)
          go(newAnnot :: xs.tail, acc)
        } else {
          go(xs, acc :+ x)
        }
    }

    arrangeAnnotationIds(go(annotations, List[BioCAnnotation]()))

  }

}
