package com.ferhtaydn.biocemid

import com.ferhtaydn.biocemid.annotators.{ TfrfAnnotatorConfig, Word2vecAnnotatorConfig }
import com.ferhtaydn.biocemid.bioc.BioC

object Main extends App {

  Console.println(
    s"""
       |------------ Welcome to the BioCreative V - BioC Task - Subtask 6 ------------
       |
       |Please press the key for:
       |1 - To generate the helper information files and tf-rf results from labelled data in $goldResultDirectory
       |2 - Annotate with TFRF/BASELINE raw files in $manualAnnotationRawDirectory
       |3 - Generate Eval results by comparing $tfrfResultDirectory and $goldResultDirectory
       |4 - Count of each method annotated in $manualAnnotationStatistics
       |5 - Annotate with WORD2VEC, raw files in $manualAnnotationRawDirectory
     """.stripMargin
  )

  val selection = scala.io.StdIn.readInt()

  if (selection == 1) {

    BioC.methodIds.foreach(createHelperFiles)
    BioC.methodIds.foreach(calculateTfrf)

    def calculateTfrf(method: String): Unit = {

      val passagesFile = s"MI${method}_annotations_passages.txt"
      val tokenizedFile = s"MI${method}_tokenized_words.txt"
      val tfRfTokenizedFile = s"MI${method}_tokenized_tf-rf.txt"

      val tokenFreqs = BioC.calcFrequenciesFromTokensFile(tokenizedFile)

      val positivePassages = read(passagesFile)
      val negativePassagesFiles = listOthers(method, "annotations_passages.txt")

      val tfrf = BioC.tfRf(tokenFreqs, positivePassages, negativePassagesFiles).sortBy(_._2).reverse

      write(tfRfTokenizedFile, stringifyTuples(tfrf))
    }

    def createHelperFiles(method: String): Unit = {

      val passagesFile = s"MI${method}_annotations_passages.txt"
      val sentencesFile = s"MI${method}_annotations_sentences.txt"
      val tokenizedFile = s"MI${method}_tokenized_words.txt"
      val tokenizedFreqsFile = s"MI${method}_tokenized_freqs.txt"

      def out(annotatedSentences: String): Unit = {
        append(passagesFile, annotatedSentences)
        append(sentencesFile, mkSentence(annotatedSentences))
        append(tokenizedFile, tokenize(mkSentence(annotatedSentences)).mkString(newline))
      }

      remove(passagesFile)
      remove(sentencesFile)
      remove(tokenizedFile)

      list(goldResultDirectory, xmlSuffix).foreach(f ⇒ out(BioC.extractAnnotatedSentences(f, method)))

      write(tokenizedFreqsFile, stringifyTuples(BioC.calcFrequenciesFromTokensFile(tokenizedFile)))

    }

  } else if (selection == 2) {

    BioC.annotate(
      manualAnnotationRawDirectory,
      xmlSuffix,
      tfrfResultSuffix,
      TfrfAnnotatorConfig(1, 0.5, 0.25)
    )

  } else if (selection == 3) {

    BioC.evaluate(goldResultDirectory, word2vecResultDirectory, xmlSuffix)

  } else if (selection == 4) {

    BioC.countOfMethods(manualAnnotationStatistics, xmlSuffix)

  } else if (selection == 5) {

    cleanPreviousResultFiles()
    generateWord2vecResultFiles()
    cleanPreviousAnnotatedFiles()

    BioC.annotate(
      manualAnnotationRawDirectory,
      xmlSuffix, word2vecAnnotationSuffix,
      Word2vecAnnotatorConfig(oaWord2vecsDirectory, word2vecResultFileSuffix, 1, 1d, 0.5)
    )

    def cleanPreviousResultFiles(): String = {
      import scala.sys.process._
      s"find $oaWord2vecsDirectory -type f -name *$word2vecResultFileSuffix" #| "xargs rm" !!

      s"find $oaWord2vecsDirectory -type f -name *$word2vecResultDedupeFileSuffix" #| "xargs rm" !!

    }

    def cleanPreviousAnnotatedFiles(): String = {
      import scala.sys.process._
      s"find $manualAnnotationRawDirectory -type f -name *$word2vecAnnotationSuffix" #| "xargs rm" !!
    }

    def dedupe(elements: Seq[(String, Double)], acc: Seq[(String, Double)]): Seq[(String, Double)] = elements match {
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

    def generateWord2vecResultFiles(): Unit = {

      lazy val methodsNames = BioC.methodsInfo.map { m ⇒
        val synonyms = m.synonym.map(x ⇒ mkStringAfterSplit(x, spaceRegex, underscore))
        val name = mkStringAfterSplit(m.name, spaceRegex, underscore)
        (m.id, if (!synonyms.contains(name)) name :: synonyms else name :: (synonyms diff List(name)))
      }.toMap

      BioC.methodIds.foreach { method ⇒

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

  } else {

    Console.println("Please select the options from 1 until 5.")
    System.exit(0)

  }

}
