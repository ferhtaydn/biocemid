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

    lazy val methodsNames: Map[String, List[String]] = methodsInfo.map(m ⇒ m.id → m.nameAndSynonymsWithUnderscore).toMap

    lazy val methodWord2vecItems: Map[String, Seq[Word2vecItem]] = methodIds.map { methodId ⇒
      list(s"$oaWord2vecsDirectory/$methodId", txtSuffix) match {
        case Nil   ⇒ methodId → Seq.empty[Word2vecItem]
        case files ⇒ methodId → combineWord2vecs(methodId, files)
      }
    }.filter(_._2.nonEmpty).toMap

    def combineWord2vecs(methodId: String, files: List[File]): Seq[Word2vecItem] = {
      val map = scala.collection.mutable.Map.empty[String, Double].withDefaultValue(0d)
      val otherMethodsNames = methodsNames - methodId
      files.foreach { file ⇒
        read(file).foreach { line ⇒
          val item = Word2vecItem.underscoredPhrases(line)
          if (!otherMethodsNames.values.exists(_.contains(item.phrase))) {
            map.get(item.phrase) match {
              case Some(cos) if cos < item.score ⇒ map(item.phrase) = item.score
              case None                          ⇒ map(item.phrase) = item.score
              case _                             ⇒ // do not modify
            }
          }
        }
      }
      // remove the ontology's name/synonyms from word2vecs list. Their score is already 1.0
      methodsNames(methodId).foreach(map.remove)
      map.toSeq.sortBy(_._2).reverse.map { case (p, s) ⇒ Word2vecItem(p, s) }
    }

    def cleanWord2vecResults(methodId: String, items: Seq[Word2vecItem]): Seq[Word2vecItem] = {

      val otherMethodItems = methodWord2vecItems - methodId
      val namesOfMethod = methodsNames(methodId)

      val results = items.foldLeft(Seq.empty[Word2vecItem]) {
        case (acc, i) ⇒
          if (namesOfMethod.exists(n ⇒ n.contains(i.phrase) || i.phrase.contains(n))) {
            acc :+ i
          } else {
            val skipItem = otherMethodItems.exists {
              case (oId, oItems) ⇒
                val namesOfOtherMethod = methodsNames(oId)
                namesOfOtherMethod.exists(n ⇒ n.contains(i.phrase) || i.phrase.contains(n)) ||
                  oItems.exists(oi ⇒ oi.phrase.equalsIgnoreCase(i.phrase) && oi.score > i.score)
            }
            if (skipItem) acc else acc :+ i
          }
      }
      results
    }

    methodWord2vecItems.foreach {
      case (methodId, items) ⇒

        val results: Seq[Word2vecItem] = cleanWord2vecResults(methodId, items)

        write(
          s"$oaWord2vecsDirectory/$methodId/$methodId-$word2vecResultFileSuffix",
          Word2vecItem.stringifyItems(results)
        )
        write(
          s"$oaWord2vecsDirectory/$methodId/$methodId-$word2vecResultDedupeFileSuffix",
          Word2vecItem.stringifyItems(dedupe(results, Seq.empty[Word2vecItem]))
        )
    }
  }

}
