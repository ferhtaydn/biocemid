package com.ferhtaydn.biocemid.annotators.word2vec

import java.io.File

import com.ferhtaydn.biocemid._

//noinspection ScalaStyle
object Word2vecHelper {

  def help(): Unit = {
    cleanPreviousResultFiles()
    cleanPreviousAnnotatedFiles()
    generateWord2vecResultFiles()
  }

  private def cleanPreviousResultFiles(): String = {
    import scala.sys.process._
    s"find $oaWord2vecsDirectory -type f -name *$word2vecResultFileSuffix" #| "xargs rm" !!

    s"find $oaWord2vecsDirectory -type f -name *$word2vecResultDedupeFileSuffix" #| "xargs rm" !!

  }

  private def cleanPreviousAnnotatedFiles(): String = {
    import scala.sys.process._
    s"find $manualAnnotationRawDirectory -type f -name *$word2vecAnnotatedSuffix" #| "xargs rm" !!
  }

  private def dedupe(elements: Seq[Word2vecItem], acc: Seq[Word2vecItem]): Seq[Word2vecItem] = elements match {
    case Nil ⇒ acc
    case (elem @ Word2vecItem(p, s)) +: tail ⇒

      val splitted = split(p, underscoreRegex)
      tail.find { case Word2vecItem(a, b) ⇒ splitted.containsSlice(split(a, underscoreRegex)) && s < b } match {
        case None ⇒
          dedupe(
            tail.filterNot { case Word2vecItem(a, b) ⇒ split(a, underscoreRegex).containsSlice(splitted) && s >= b },
            acc :+ elem
          )
        case Some(x) ⇒ dedupe(tail, acc)
      }
  }

  private def generateWord2vecResultFiles(): Unit = {

    lazy val methodsNames: Map[String, List[String]] = methodsInfo.map { m ⇒
      val synonyms = m.synonym.map(x ⇒ mkStringAfterSplit(x, spaceRegex, underscore))
      val name = mkStringAfterSplit(m.name, spaceRegex, underscore)
      (m.id, if (!synonyms.contains(name)) name :: synonyms else name :: (synonyms diff List(name)))
    }.toMap

    lazy val methodPhraseScores: Map[String, Seq[Word2vecItem]] = methodIds.map { methodId ⇒
      list(s"$oaWord2vecsDirectory/$methodId", txtSuffix) match {
        case Nil ⇒ methodId → Seq.empty[Word2vecItem]
        case files ⇒
          methodId → combineWord2vecs(methodId, files)
      }
    }.filter(_._2.nonEmpty).toMap

    def combineWord2vecs(methodId: String, files: List[File]): Seq[Word2vecItem] = {
      val map = scala.collection.mutable.Map.empty[String, Double].withDefaultValue(0d)
      val otherMethodsNames = methodsNames - methodId
      files.foreach { file ⇒
        read(file).foreach { line ⇒
          val ps = Word2vecItem.underscoredPhrases(line)
          if (!otherMethodsNames.values.exists(_.contains(ps.phrase))) {
            map.get(ps.phrase) match {
              case Some(cos) if cos < ps.score ⇒ map(ps.phrase) = ps.score
              case None                        ⇒ map(ps.phrase) = ps.score
              case _                           ⇒ // do not modify
            }
          }
        }
      }
      // remove the ontology's name/synonyms from word2vecs list. Their score is already 1.0
      methodsNames(methodId).foreach(map.remove)
      map.toSeq.sortBy(_._2).reverse.map { case (p, s) ⇒ Word2vecItem(p, s) }
    }

    methodPhraseScores.foreach {
      case (methodId, pss) ⇒

        write(
          s"$oaWord2vecsDirectory/$methodId/$methodId-$word2vecResultFileSuffix",
          Word2vecItem.stringifyItems(pss)
        )
        write(
          s"$oaWord2vecsDirectory/$methodId/$methodId-$word2vecResultDedupeFileSuffix",
          Word2vecItem.stringifyItems(dedupe(pss, Seq.empty[Word2vecItem]))
        )
    }
  }

}
