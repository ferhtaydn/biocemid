package com.ferhtaydn.biocreative5.task1.subtask6

import bioc.util.CopyConverter
import bioc.{ BioCAnnotation, BioCLocation, BioCPassage, BioCSentence }

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * This class is used for to look for the previous and next sentences of the annotated sentence.
 * This converter uses also the word2vecs of the methods.
 */
//noinspection ScalaStyle
class SentenceConverter3(word2vecsDir: String) extends CopyConverter {

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

          lazy val word2vecs: Seq[(String, Double)] = IO.list(s"$word2vecsDir/$id", IO.word2vecResultFileSuffix).headOption match {
            case Some(file) ⇒
              IO.read(file).map { line ⇒
                val scoreString = line.split(",").last
                val word = line.dropRight(scoreString.length + 1)
                val phrase = word.split("_").mkString(" ")
                val score = scoreString.toDouble
                phrase -> score
              }
            case None ⇒ Seq()
          }

          val synonymNgram = info.nameAndSynonyms.flatMap { s ⇒
            val size = s.split("\\s").size
            if (size > 1) {
              Utils.mkNgram(words, size).filter(_.equalsIgnoreCase(s))
            } else {
              words.filter(_.equalsIgnoreCase(s))
            }
          }

          val matchingVectors = word2vecs.flatMap {
            case (phrase, score) ⇒
              val size = phrase.split("\\s").size
              if (size > 1) {
                Utils.mkNgram(words, size).filter(_.equalsIgnoreCase(phrase))
              } else {
                words.filter(_.equalsIgnoreCase(phrase))
              }
          }
          val word2vecResults = word2vecs.filter { case (p, s) ⇒ matchingVectors.contains(p) }

          def dedupe(elements: Seq[(String, Double)], acc: Seq[(String, Double)]): Seq[(String, Double)] = elements match {
            case Nil ⇒ acc
            case (elem @ (p, s)) +: tail ⇒

              tail.find { case (a, b) ⇒ (a.contains(p) || p.contains(a)) && s < b } match {
                case None    ⇒ dedupe(tail.filterNot { case (a, b) ⇒ a.contains(p) || p.contains(a) }, acc :+ elem)
                case Some(x) ⇒ dedupe(tail, acc)
              }
          }

          def filterMaxLengthMatch(elements: Seq[(String, Double)], acc: Seq[(String, Double)]): Seq[(String, Double)] = elements match {
            case Nil ⇒ acc
            case (elem @ (p, s)) +: tail ⇒

              tail.find { case (a, b) ⇒ a.contains(p) } match {
                case None ⇒ acc.find { case (x, y) ⇒ x.contains(p) } match {
                  case None    ⇒ filterMaxLengthMatch(tail, acc :+ elem)
                  case Some(t) ⇒ filterMaxLengthMatch(tail, acc)
                }
                case Some(x) ⇒ filterMaxLengthMatch(tail, acc)
              }
          }

          //todo decide here
          //val n = if (synonymNgram.nonEmpty) synonymNgram.map(s ⇒ s -> 1d).map(_._2).sum else 0d
          val n = if (synonymNgram.nonEmpty) 1d else 0d
          //val w = if (word2vecResults.nonEmpty) filterMaxLengthMatch(word2vecResults, Seq()).map(_._2).sum else 0d
          val w = if (word2vecResults.nonEmpty) word2vecResults.map(_._2).sum else 0d

          MethodWeight(id, n + w)

      }.filter(_.weight > 0.0).sortWith(_.weight > _.weight)

    }

    def setWeights(sentence: BioCSentence, methodWeights: List[MethodWeight]): BioCSentence = {

      methodWeights match {
        case Nil ⇒ sentence
        case mw :: mws ⇒
          if (mw.weight >= 1.0) {
            //todo change threshold or look for duplicates, not only the first one.
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
    }

    setWeights(sentence, calculateMethodWeights(Utils.tokenize(sentence.getText)))

  }

  private def annotatePreviousAndNextSentences(annotatedSentences: mutable.MutableList[BioCSentence]) = {

    def annotateInfon(sentence: BioCSentence, targetSentence: BioCSentence) = {

      val sentenceAnnotation = sentence.getAnnotations.head.getInfon("PSIMI")

      import MethodWeight.fromInfons
      val targetSentenceInfon: List[MethodWeight] = targetSentence.getInfons.toMap

      //todo decide here
      targetSentenceInfon.find(mw ⇒ mw.id.equals(sentenceAnnotation) && mw.weight >= 0.5).map { mw ⇒
        val annotationInfons = Map("type" -> "ExperimentalMethod", "PSIMI" -> mw.id)
        val out: BioCAnnotation = new BioCAnnotation
        out.setInfons(annotationInfons)
        out.setText(targetSentence.getText)
        out.setLocation(targetSentence.getOffset, targetSentence.getText.length)
        out
      }
    }

    annotatedSentences.zipWithIndex.foreach {

      case (sentence, index) ⇒

        if (sentence.getAnnotations.nonEmpty && sentence.getInfons.isEmpty) {

          //todo decide here
          ((index - 2) to (index + 2)).foreach { i ⇒
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
