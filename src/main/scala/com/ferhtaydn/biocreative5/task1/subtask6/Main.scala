package com.ferhtaydn.biocreative5.task1.subtask6

object Main extends App {

  Console.println(

    s"""
      |------------ Welcome to the BioCreative V - BioC Task - Subtask 6 ------------
      |
      |Please press the key for:
      |
      |1 - To generate the helper information files and tf-rf results from training data in ${IO.manualAnnotationStatistics}
      |
      |2 - Annotate with TFRF/BASELINE raw files in ${IO.manualAnnotationRawDirectory}
      |
      |3 - Generate Eval results in ${IO.goldResultDirectory}
      |
      |4 - Count of each method annotated in ${IO.manualAnnotationStatistics}
      |
      |5 - Annotate with WORD2VEC, raw files in ${IO.manualAnnotationRawDirectory}
      |
    """.stripMargin

  )

  val selection = scala.io.StdIn.readInt()

  if (selection == 0) {

    val list = BioC.methodsInfo.map { method ⇒
      s"${method.id} ${method.nameAndSynonyms.map(x ⇒ x.split("\\s").mkString("_")).mkString(" ")}"
    }

    IO.write("files/word2vec_distance_files_run_params.txt", list.mkString("\n"))

  } else if (selection == 1) {

    BioC.methodIds.foreach(createHelperFiles)
    BioC.methodIds.foreach(calculateTfrf)

    def calculateTfrf(method: String): Unit = {

      println(s"method id: $method")

      val passagesFile = s"MI${method}_annotations_passages.txt"
      val tokenizedFile = s"MI${method}_tokenized_words.txt"
      val tfRfTokenizedFile = s"MI${method}_tokenized_tf-rf.txt"

      val tokenFreqs = BioC.calcFrequenciesFromTokensFile(tokenizedFile)

      val positivePassages = IO.read(passagesFile)
      val negativePassagesFiles = IO.listOthers(method, "annotations_passages.txt")

      val tfrf = BioC.tfRf(tokenFreqs, positivePassages, negativePassagesFiles).sortBy(_._2).reverse

      IO.write(tfRfTokenizedFile, Utils.stringifyTuples(tfrf))
    }

    def createHelperFiles(method: String): Unit = {

      val passagesFile = s"MI${method}_annotations_passages.txt"
      val sentencesFile = s"MI${method}_annotations_sentences.txt"
      val tokenizedFile = s"MI${method}_tokenized_words.txt"
      val tokenizedFreqsFile = s"MI${method}_tokenized_freqs.txt"

      def out(annotatedSentences: String): Unit = {
        IO.append(passagesFile, annotatedSentences)
        IO.append(sentencesFile, Utils.mkSentence(annotatedSentences))
        IO.append(tokenizedFile, Utils.tokenize(Utils.mkSentence(annotatedSentences)).mkString("\n"))
      }

      IO.remove(passagesFile)
      IO.remove(sentencesFile)
      IO.remove(tokenizedFile)

      IO.list(IO.manualAnnotationStatistics, IO.xmlSuffix).foreach(f ⇒ out(BioC.extractAnnotatedSentences(f, method)))

      IO.write(tokenizedFreqsFile, Utils.stringifyTuples(BioC.calcFrequenciesFromTokensFile(tokenizedFile)))

    }

  } else if (selection == 2) {

    BioC.annotateWithTfrf(IO.manualAnnotationRawDirectory,
      IO.xmlSuffix,
      IO.baselineResultSuffix,
      tfrfConfigs = (false, 1, 0.5, 0.25)
    )

  } else if (selection == 3) {

    BioC.evaluate(IO.goldResultDirectory, IO.word2vecResultDirectory, IO.xmlSuffix)

  } else if (selection == 4) {

    BioC.countOfMethods(IO.manualAnnotationStatistics, IO.xmlSuffix)

  } else if (selection == 5) {

    cleanPreviousResultFiles()
    generateWord2vecResultFiles()
    cleanPreviousAnnotatedFiles()

    BioC.annotateWithWord2vec(
      IO.manualAnnotationRawDirectory,
      IO.xmlSuffix, IO.word2vecAnnotationSuffix,
      (IO.oaWord2vecsDirectory, IO.word2vecResultDedupeFileSuffix, 1, 0.9, 0.5)
    )

    def cleanPreviousResultFiles(): String = {
      import scala.sys.process._
      s"find ${IO.oaWord2vecsDirectory} -type f -name *${IO.word2vecResultFileSuffix}" #| "xargs rm" !!

      s"find ${IO.oaWord2vecsDirectory} -type f -name *${IO.word2vecResultDedupeFileSuffix}" #| "xargs rm" !!

    }

    def cleanPreviousAnnotatedFiles(): String = {
      import scala.sys.process._
      s"find ${IO.manualAnnotationRawDirectory} -type f -name *${IO.word2vecAnnotationSuffix}" #| "xargs rm" !!
    }

    def dedupe(elements: Seq[(String, Double)], acc: Seq[(String, Double)]): Seq[(String, Double)] = elements match {
      case Nil ⇒ acc
      case (elem @ (p, s)) +: tail ⇒

        val splitted = split(p)
        tail.find { case (a, b) ⇒ splitted.containsSlice(split(a)) && s < b } match {
          case None    ⇒ dedupe(tail.filterNot { case (a, b) ⇒ split(a).containsSlice(splitted) && s >= b }, acc :+ elem)
          case Some(x) ⇒ dedupe(tail, acc)
        }
    }

    def split(s: String): List[String] = s.split("_").toList

    def generateWord2vecResultFiles(): Unit = {

      lazy val methodsNames = BioC.methodsInfo.map { m ⇒
        val synonyms = m.synonym.map(x ⇒ x.split(" ").mkString("_"))
        val name = m.name.split(" ").mkString("_")
        (m.id, if (!synonyms.contains(name)) name :: synonyms else name :: (synonyms diff List(name)))
      }.toMap

      BioC.methodIds.foreach { method ⇒

        IO.list(s"${IO.oaWord2vecsDirectory}/$method", IO.txtSuffix) match {
          case Nil ⇒ //do nothing
          case files ⇒
            val map = scala.collection.mutable.Map.empty[String, Double].withDefaultValue(0d)

            val otherMethodsNames = methodsNames - method

            files.foreach { file ⇒

              IO.read(file).foreach { line ⇒

                val cosineString = line.split(",").last
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
            IO.write(s"${IO.oaWord2vecsDirectory}/$method/$method-${IO.word2vecResultFileSuffix}",
              Utils.stringifyTuples(map.toSeq.sortBy(_._2).reverse))

            IO.write(s"${IO.oaWord2vecsDirectory}/$method/$method-${IO.word2vecResultDedupeFileSuffix}",
              Utils.stringifyTuples(dedupe(map.toSeq, Seq()).sortBy(_._2).reverse))

        }
      }
    }

  } else {

    Console.println("Please select the options from 1 until 5.")
    System.exit(0)

  }

}
