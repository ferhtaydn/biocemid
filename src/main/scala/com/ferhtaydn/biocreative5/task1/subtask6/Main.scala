package com.ferhtaydn.biocreative5.task1.subtask6

object Main extends App {

  Console.println(

    s"""
      |------------ Welcome to the BioCreative V - BioC Task - Subtask 6 ------------
      |
      |Please press the key for:
      |
      |1 - To generate the helper information files and tf-rf results from training data in ${IO.annotatedDirectory}
      |
      |2 - To get tf-rf results from PSI-MI ontology definitions
      |
      |3 - Annotate the raw BioC files in ${IO.annotationDirectory}
      |
      |4 - Generate Eval results in ${IO.algoResultsDirectory}
      |
      |5 - Count of each method annotated in ${IO.algoResultsDirectory}
      |
      |6 - To generate bc3 word2vec results for each method name and synonym
      |
      |7 - Gather the bc3 word2vec results for each method
      |
      |8 - Gather the open access word2vec results for each method
      |
    """.stripMargin

  )

  val selection = scala.io.StdIn.readInt()

  if (selection == 1) {

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

      IO.list(IO.annotatedDirectory, IO.xmlSuffix).foreach(f ⇒ out(BioC.extractAnnotatedSentences(f, method)))

      IO.write(tokenizedFreqsFile, Utils.stringifyTuples(BioC.calcFrequenciesFromTokensFile(tokenizedFile)))

    }

  } else if (selection == 2) {

    BioC.methodsInfo.foreach { m ⇒

      val tfRfTokenizedFile = s"MI${m.id}_ontology_tf-rf.txt"

      val tokenFreqs = BioC.calcTokenFrequencies(Utils.tokenize(m.definition))

      val negativePassages = BioC.methodsInfo.filter(_.id != m.id).map(_.definition).map(Utils.tokenize(_).toSet).toSeq

      val tfrf = BioC.tfRfOntology(tokenFreqs, Seq(m.definition), negativePassages).sortBy(_._2).reverse

      IO.write(tfRfTokenizedFile, Utils.stringifyTuples(tfrf))

    }

  } else if (selection == 3) {

    BioC.annotate(IO.annotationDirectory, IO.xmlSuffix, "passages_with_exp_methods_with_before_after.xml")

  } else if (selection == 4) {

    BioC.evaluate(IO.annotatedDirectory, IO.algoResultsDirectory, IO.xmlSuffix)

  } else if (selection == 5) {

    BioC.countOfMethods(IO.annotatedDirectory, IO.xmlSuffix)

  } else if (selection == 6) {

    deleteWord2vecsDirectory()
    generateWord2vecs()

    def deleteWord2vecsDirectory(): Unit = {
      import sys.process._
      //clean the word2vecs file
      s"find . -name ${IO.bc3Word2vecsDirectory}" #| "xargs rm -r" !!

      s"mkdir ${IO.bc3Word2vecsDirectory}".!!
    }

    def generateWord2vecs(): Unit = {

      val methods = BioC.methodsInfo.map { m ⇒
        val synonyms = m.synonym.map(x ⇒ x.split(" ").mkString("_"))
        val name = m.name.split(" ").mkString("_")
        m.id :: (if (!synonyms.contains(name)) name :: synonyms else name :: (synonyms diff List(name)))
      }

      import sys.process._
      methods.foreach { m ⇒
        val seq = Seq("/Users/aydinf/Desktop/word2vec_extension/distance_files",
          "/Users/aydinf/Desktop/bc3_word2vec_results/phrase1_eval/bc3_phrase1_vectors.bin") ++ m
        Process(seq, new java.io.File(IO.bc3Word2vecsDirectory)).!!
      }
    }

  } else if (selection == 7) {

    cleanPreviousResultFiles
    generateWord2vecResultFiles()
    cleanPreviousAnnotatedFiles
    BioC.annotate(IO.bc3Word2vecAnnotationDirectory, IO.xmlSuffix, IO.word2vecAnnotationSuffix, Some(IO.oaWord2vecsDirectory))

    def cleanPreviousResultFiles: String = {
      import scala.sys.process._
      s"find ${IO.bc3Word2vecsDirectory} -type f -name *${IO.word2vecResultFileSuffix}" #| "xargs rm" !!
    }

    def cleanPreviousAnnotatedFiles: String = {
      import scala.sys.process._
      s"find ${IO.bc3Word2vecAnnotationDirectory} -type f -name *${IO.word2vecAnnotationSuffix}" #| "xargs rm" !!
    }

    def generateWord2vecResultFiles(): Unit = {

      def maxMinNormalization(data: Seq[(String, Double)], itemCount: Int) = {
        val partial = data.sortBy(_._2).reverse.take(itemCount)
        val min = partial.last._2
        val max = partial.head._2
        partial.map { case (w, c) ⇒ (w, (c - min) / (max - min)) }
      }

      lazy val methodsNames = BioC.methodsInfo.map { m ⇒
        val synonyms = m.synonym.map(x ⇒ x.split(" ").mkString("_"))
        val name = m.name.split(" ").mkString("_")
        (m.id, if (!synonyms.contains(name)) name :: synonyms else name :: (synonyms diff List(name)))
      }.toMap

      BioC.methodsInfo.foreach { method ⇒

        IO.list(s"${IO.bc3Word2vecsDirectory}/${method.id}", IO.txtSuffix) match {
          case Nil ⇒ //do nothing
          case files ⇒
            val map = scala.collection.mutable.Map.empty[String, Double].withDefaultValue(0d)
            val list = scala.collection.mutable.MutableList.empty[(String, Double)]

            val otherMethodsNames = methodsNames - method.id

            files.foreach { file ⇒
              IO.read(file).foreach { line ⇒
                val cosineString = line.split(",").last
                val word = line.dropRight(cosineString.length + 1)
                val cosine = cosineString.toDouble
                if (!otherMethodsNames.values.exists(_.contains(word))) list += word -> cosine
              }
            }

            list.foreach {
              case (word, cosine) ⇒
                map.get(word) match {
                  case Some(cos) ⇒ map.update(word, map(word) + cosine)
                  case None      ⇒ map(word) = cosine
                }
            }

            val numberOfVectors = 101
            //remove the method name and synonyms, those will be checked in converter.
            methodsNames(method.id).foreach(map.remove)
            val maxMinNormalized = maxMinNormalization(map.toSeq, numberOfVectors)

            IO.write(s"${IO.bc3Word2vecsDirectory}/${method.id}/${method.id}-${IO.word2vecResultFileSuffix}", Utils.stringifyTuples(maxMinNormalized))
        }
      }
    }

  } else if (selection == 8) {

    cleanPreviousResultFiles
    generateWord2vecResultFiles()
    cleanPreviousAnnotatedFiles
    BioC.annotate(IO.oaWord2vecAnnotationDirectory, IO.xmlSuffix, IO.word2vecAnnotationSuffix, Some(IO.oaWord2vecsDirectory))

    def cleanPreviousResultFiles: String = {
      import scala.sys.process._
      s"find ${IO.oaWord2vecsDirectory} -type f -name *${IO.word2vecResultFileSuffix}" #| "xargs rm" !!
    }

    def cleanPreviousAnnotatedFiles: String = {
      import scala.sys.process._
      s"find ${IO.oaWord2vecAnnotationDirectory} -type f -name *${IO.word2vecAnnotationSuffix}" #| "xargs rm" !!
    }

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
        }
      }
    }

  } else {

    Console.println("Please select the options from 1 until 8.")
    System.exit(0)

  }

}
