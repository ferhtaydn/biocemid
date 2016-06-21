package com.ferhtaydn.biocemid.annotators.word2vec

import java.io.File

import com.ferhtaydn.biocemid._

//noinspection ScalaStyle
object Word2vecHelper {

  def help(config: Word2vecAnnotatorConfig): Unit = {
    cleanPreviousResultFiles(config)
    cleanPreviousAnnotatedFiles(config)
    generateRawWord2vecResultFiles(config)
    generateWord2vecResultFiles(config)
    generateDedupeEnhancedResultFiles(config)
    //generateRawLatexTableContext("0018")
    //generateDedupeLatexTableContext("0018")
  }

  private def cleanPreviousResultFiles(config: Word2vecAnnotatorConfig): String = {
    import scala.sys.process._
    s"find ${config.w2vDir} -type f -name *$word2vecResultFileSuffix" #| "xargs rm" !!

    s"find ${config.w2vDir} -type f -name *$word2vecResultDedupeFileSuffix" #| "xargs rm" !!

    s"find ${config.w2vDir} -type f -name *$word2vecResultDedupeEnhancedFileSuffix" #| "xargs rm" !!

    s"find ${config.w2vDir} -type f -name *$word2vecResultRawFileSuffix" #| "xargs rm" !!
  }

  private def cleanPreviousAnnotatedFiles(config: Word2vecAnnotatorConfig): String = {
    import scala.sys.process._
    s"find ${config.rawDirectory} -type f -name *${config.outputFileSuffix}" #| "xargs rm" !!
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

  private def generateRawWord2vecResultFiles(config: Word2vecAnnotatorConfig): Unit = {

    def combineRawWord2vecs(methodId: String, files: List[File]): Seq[Word2vecItem] = {
      val map = scala.collection.mutable.Map.empty[String, Double].withDefaultValue(0d)
      files.foreach { file ⇒
        read(file).foreach { line ⇒
          val item = Word2vecItem.underscoredPhrases(line)
          map.get(item.phrase) match {
            case Some(cos) if cos < item.score ⇒ map(item.phrase) = item.score
            case None                          ⇒ map(item.phrase) = item.score
            case _                             ⇒ // do not modify
          }
        }
      }
      map.toSeq.sortBy(_._2).reverse.map { case (p, s) ⇒ Word2vecItem(p, s) }
    }

    lazy val rawMethodWord2vecItems: Map[String, Seq[Word2vecItem]] = methodIds.map { methodId ⇒
      list(s"${config.w2vDir}/$methodId", txtSuffix) match {
        case Nil   ⇒ methodId → Seq.empty[Word2vecItem]
        case files ⇒ methodId → combineRawWord2vecs(methodId, files)
      }
    }.filter(_._2.nonEmpty).toMap

    rawMethodWord2vecItems.foreach {
      case (methodId, items) ⇒
        write(
          s"${config.w2vDir}/$methodId/$methodId-$word2vecResultRawFileSuffix",
          Word2vecItem.stringifyItems(items)
        )
    }
  }

  private def generateRawLatexTableContext(methodId: String): Unit = {

    val rawFileName = s"$methodId-$word2vecResultRawFileSuffix"
    val fileName = s"$methodId-$word2vecResultFileSuffix"

    val raw = read(s"$oaWord2vecsDirectory/$methodId/$rawFileName").map { line ⇒
      Word2vecItem.underscoredPhrases(line)
    }
    val normal = read(s"$oaWord2vecsDirectory/$methodId/$fileName").map { line ⇒
      Word2vecItem.underscoredPhrases(line)
    }

    val diff = raw diff normal

    val a = raw.take(45)
    val b = raw.slice(45, 90)

    a zip b foreach {
      case (x, y) ⇒
        val itX = if (diff.contains(x))
          s"\\textit{${x.phrase}} & \\textit{${x.score}} & "
        else
          s"\\textbf{${x.phrase}} & \\textbf{${x.score}} & "

        val itY = if (diff.contains(y))
          s"\\textit{${y.phrase}} & \\textit{${y.score}} \\\\"
        else
          s"\\textbf{${y.phrase}} & \\textbf{${y.score}} \\\\"

        println(s"$itX $itY")
    }
  }

  private def generateDedupeLatexTableContext(methodId: String): Unit = {

    val dedupeFileName = s"$methodId-$word2vecResultDedupeFileSuffix"
    val fileName = s"$methodId-$word2vecResultFileSuffix"

    val dedupe = read(s"$oaWord2vecsDirectory/$methodId/$dedupeFileName").map { line ⇒
      Word2vecItem.underscoredPhrases(line)
    }
    val normal = read(s"$oaWord2vecsDirectory/$methodId/$fileName").map { line ⇒
      Word2vecItem.underscoredPhrases(line)
    }

    val diff = normal diff dedupe

    val a = normal.take(45)
    val b = normal.slice(45, 90)

    a zip b foreach {
      case (x, y) ⇒
        val itX = if (diff.contains(x))
          s"\\textit{${x.phrase}} & \\textit{${x.score}} & "
        else
          s"\\textbf{${x.phrase}} & \\textbf{${x.score}} & "

        val itY = if (diff.contains(y))
          s"\\textit{${y.phrase}} & \\textit{${y.score}} \\\\"
        else
          s"\\textbf{${y.phrase}} & \\textbf{${y.score}} \\\\"

        println(s"$itX $itY")
    }
  }

  private def generateWord2vecResultFiles(config: Word2vecAnnotatorConfig): Unit = {

    val methodsNames: Map[String, List[String]] = methodsInfo.map { m ⇒
      val names = if (config.pureBaseline) m.pureNameAndSynonymsWithUnderscore else m.nameAndSynonymsWithUnderscore
      m.id → names
    }.toMap

    def methodNamesToWord2VecItems(id: String): List[Word2vecItem] = methodsNames(id).map(Word2vecItem(_, 1d))

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
      map.toSeq.sortBy(_._2).reverse.map { case (p, s) ⇒ Word2vecItem(p, s) }
    }

    val methodWord2vecItems: Map[String, Seq[Word2vecItem]] = methodIds.map { methodId ⇒
      list(s"${config.w2vDir}/$methodId", txtSuffix) match {
        case Nil   ⇒ methodId → Seq.empty[Word2vecItem]
        case files ⇒ methodId → combineWord2vecs(methodId, files)
      }
    }.filter(_._2.nonEmpty).toMap

    def cleanWord2vecResults(methodId: String, items: Seq[Word2vecItem]): Seq[Word2vecItem] = {

      val otherMethodItems = methodWord2vecItems - methodId
      val namesOfMethod = methodsNames(methodId)

      val results = items.foldLeft(Seq.empty[Word2vecItem]) {
        case (acc, i) ⇒
          val skipItem = namesOfMethod.find(n ⇒ n.contains(i.phrase) || i.phrase.contains(n)) match {
            case None ⇒
              otherMethodItems.exists {
                case (oId, oItems) ⇒
                  val namesOfOtherMethod = methodsNames(oId)
                  namesOfOtherMethod.exists(n ⇒ n.contains(i.phrase) || i.phrase.contains(n)) ||
                    oItems.exists(oi ⇒ oi.phrase.equalsIgnoreCase(i.phrase) && oi.score > i.score)
              }
            case Some(name) ⇒
              otherMethodItems.exists {
                case (oId, oItems) ⇒
                  val namesOfOtherMethod = methodsNames(oId)
                  namesOfOtherMethod.find(n ⇒ i.phrase.contains(n)) match {
                    case None ⇒ false
                    case Some(oName) ⇒
                      if (oName.contains(name) || name.contains(oName)) {
                        oName.length > name.length
                      } else {
                        oItems.exists(oi ⇒ oi.phrase.equalsIgnoreCase(i.phrase) && oi.score > i.score)
                      }
                  }
              }
          }
          if (skipItem) acc else acc :+ i
      }
      results
    }

    methodWord2vecItems.foreach {
      case (methodId, items) ⇒

        val cleanedResults: Seq[Word2vecItem] = cleanWord2vecResults(methodId, items)

        val results = cleanedResults.filterNot(i ⇒ methodsNames(methodId).contains(i.phrase))
        val dedupeResults = dedupe(methodNamesToWord2VecItems(methodId) ++ results, Seq.empty[Word2vecItem])
          .filterNot(i ⇒ methodsNames(methodId).contains(i.phrase))

        write(
          s"${config.w2vDir}/$methodId/$methodId-$word2vecResultFileSuffix",
          Word2vecItem.stringifyItems(results)
        )
        write(
          s"${config.w2vDir}/$methodId/$methodId-$word2vecResultDedupeFileSuffix",
          Word2vecItem.stringifyItems(dedupeResults)
        )
    }
  }

  def generateDedupeEnhancedResultFiles(config: Word2vecAnnotatorConfig): Unit = {

    lazy val methodWord2vecItems: Map[String, Seq[Word2vecItem]] = methodIds.map { methodId ⇒
      methodId → read(s"${config.w2vDir}/$methodId/$methodId-$word2vecResultDedupeFileSuffix")
        .map(line ⇒ Word2vecItem.underscoredPhrases(line))
    }.filter(_._2.nonEmpty).toMap

    lazy val redundantKeywords = List("assay", "assays", "experiment", "experiments", "analysis", "system", "analyses")

    def expandMethodNames(items: Seq[Word2vecItem]): Seq[Word2vecItem] = {
      items.filter(_.score >= config.mainThreshold)
        .foldLeft(Seq.empty[Word2vecItem]) {
          case (acc, i) ⇒
            redundantKeywords.find(_.equalsIgnoreCase(i.phrase.split(underscoreRegex).last)) match {
              case None    ⇒ acc :+ i
              case Some(a) ⇒ acc :+ i.copy(i.phrase.split(underscoreRegex + a).head)
            }
        }
    }

    lazy val expandedMethodsNames: Map[String, List[String]] = methodsInfo.map { m ⇒
      val names = if (config.pureBaseline) m.pureNameAndSynonymsWithUnderscore else m.nameAndSynonymsWithUnderscore

      val ex = methodWord2vecItems.get(m.id) match {
        case None    ⇒ Nil
        case Some(x) ⇒ expandMethodNames(x)
      }

      m.id → (names ++ ex.map(_.phrase)).distinct
    }.toMap

    def cleanDedupeResults(methodId: String, items: Seq[Word2vecItem]): Seq[Word2vecItem] = {

      val otherMethodItems = methodWord2vecItems - methodId
      val namesOfMethod = expandedMethodsNames(methodId)

      val results = items.foldLeft(Seq.empty[Word2vecItem]) {
        case (acc, i) ⇒
          val skipItem = namesOfMethod.find(n ⇒ n.contains(i.phrase) || i.phrase.contains(n)) match {
            case None ⇒
              otherMethodItems.exists {
                case (oId, oItems) ⇒
                  val namesOfOtherMethod = expandedMethodsNames(oId)
                  namesOfOtherMethod.exists(n ⇒ n.contains(i.phrase) || i.phrase.contains(n)) ||
                    oItems.exists(oi ⇒ oi.phrase.equalsIgnoreCase(i.phrase) && oi.score > i.score)
              }
            case Some(name) ⇒
              otherMethodItems.exists {
                case (oId, oItems) ⇒
                  val namesOfOtherMethod = expandedMethodsNames(oId)
                  namesOfOtherMethod.find(n ⇒ i.phrase.contains(n)) match {
                    case None ⇒ false
                    case Some(oName) ⇒
                      if (oName.contains(name) || name.contains(oName)) {
                        oName.length > name.length
                      } else {
                        oItems.exists(oi ⇒ oi.phrase.equalsIgnoreCase(i.phrase) && oi.score > i.score)
                      }
                  }
              }
          }
          if (skipItem) acc else acc :+ i
      }
      results
    }

    methodWord2vecItems.foreach {
      case (methodId, items) ⇒

        val cleanedResults: Seq[Word2vecItem] = cleanDedupeResults(methodId, items)

        val names = expandedMethodsNames(methodId).map(Word2vecItem(_, 1d))

        val newNames = expandMethodNames(items)

        val dedupeResults = dedupe(names ++ cleanedResults, Seq.empty[Word2vecItem])
          .filterNot(i ⇒ expandedMethodsNames(methodId).contains(i.phrase))

        write(
          s"${config.w2vDir}/$methodId/$methodId-$word2vecResultDedupeEnhancedFileSuffix",
          Word2vecItem.stringifyItems(newNames ++ dedupeResults.distinct)
        )
    }
  }

}
