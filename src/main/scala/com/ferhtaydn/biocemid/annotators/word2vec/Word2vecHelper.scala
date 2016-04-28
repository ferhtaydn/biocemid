package com.ferhtaydn.biocemid.annotators.word2vec

import com.ferhtaydn.biocemid._

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

  private def dedupe(elements: Seq[(String, Double)], acc: Seq[(String, Double)]): Seq[(String, Double)] = elements match {
    case Nil ⇒ acc
    case (elem @ (p, s)) +: tail ⇒

      val splitted = split(p, underscoreRegex)
      tail.find { case (a, b) ⇒ splitted.containsSlice(split(a, underscoreRegex)) && s < b } match {
        case None ⇒
          dedupe(
            tail.filterNot { case (a, b) ⇒ split(a, underscoreRegex).containsSlice(splitted) && s >= b },
            acc :+ elem
          )
        case Some(x) ⇒ dedupe(tail, acc)
      }
  }

  private def generateWord2vecResultFiles(): Unit = {

    lazy val methodsNames = methodsInfo.map { m ⇒
      val synonyms = m.synonym.map(x ⇒ mkStringAfterSplit(x, spaceRegex, underscore))
      val name = mkStringAfterSplit(m.name, spaceRegex, underscore)
      (m.id, if (!synonyms.contains(name)) name :: synonyms else name :: (synonyms diff List(name)))
    }.toMap

    methodIds.foreach { method ⇒

      list(s"$oaWord2vecsDirectory/$method", txtSuffix) match {
        case Nil ⇒ //do nothing
        case files ⇒
          val map = scala.collection.mutable.Map.empty[String, Double].withDefaultValue(0d)

          val otherMethodsNames = methodsNames - method

          files.foreach { file ⇒
            read(file).foreach { line ⇒

              val cosineString = split(line, commaRegex).last
              val word = line.dropRight(cosineString.length + 1)
              val cosine = cosineString.toDouble

              if (!otherMethodsNames.values.exists(_.contains(word))) {
                map.get(word) match {
                  case Some(cos) if cos < cosine ⇒ map(word) = cosine
                  case None                      ⇒ map(word) = cosine
                  case _                         ⇒ // do not modify
                }
              }
            }
          }
          // remove the name/synonyms from word2vecs list.
          methodsNames(method).foreach(map.remove)
          write(
            s"$oaWord2vecsDirectory/$method/$method-$word2vecResultFileSuffix",
            stringifyTuples(map.toSeq.sortBy(_._2).reverse)
          )
          write(
            s"$oaWord2vecsDirectory/$method/$method-$word2vecResultDedupeFileSuffix",
            stringifyTuples(dedupe(map.toSeq, Seq()).sortBy(_._2).reverse)
          )
      }
    }
  }

}
