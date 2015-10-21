package com.ferhtaydn.biocreative5.task1.subtask6

object Main extends App {

  val annotatedDirectory = "manual_annotated_data_set"
  val annotationDirectory = "xml/bc5_dataset"
  val algoResultsDirectory = "annotated_before_after_results"

  Console.println(

    s"""
      |------------ Welcome to the BioCreative V - BioC Task - Subtask 6 ------------
      |
      |Please press the key for:
      |
      |1 - To generate the helper information files and tf-rf results from training data in $annotatedDirectory
      |
      |2 - Annotate the raw BioC files in $annotationDirectory
      |
      |3 - Generate Eval results in $algoResultsDirectory
      |
      |4 - Count of each method annotated in $algoResultsDirectory
      |
    """.stripMargin

  )

  val selection = scala.io.StdIn.readInt()

  if (selection == 1) {

    Console.println("Please enter the PSIMI code for method. e.g. 0006")

    val method = scala.io.StdIn.readLine()

    createHelperFiles(method)

  } else if (selection == 2) {

    BioC.annotate(annotationDirectory, ".xml", "passages_with_exp_methods_with_before_after.xml")

  } else if (selection == 3) {

    BioC.evaluate(annotatedDirectory, algoResultsDirectory, ".xml")

  } else if (selection == 4) {

    BioC.countOfMethods(annotatedDirectory, ".xml")

  } else {

    Console.println("Please select the options from 1 until 4.")
    System.exit(0)

  }

  def createHelperFiles(method: String): Unit = {

    val passagesFile = s"MI${method}_annotations_passages.txt"
    val sentencesFile = s"MI${method}_annotations_sentences.txt"
    val tokenizedFile = s"MI${method}_tokenized_words.txt"
    val tokenizedFreqsFile = s"MI${method}_tokenized_freqs.txt"
    val tfRfTokenizedFile = s"MI${method}_tokenized_tf-rf.txt"

    def out(annotatedSentences: String): Unit = {
      IO.append(passagesFile, annotatedSentences)
      IO.append(sentencesFile, Utils.mkSentence(annotatedSentences))
      IO.append(tokenizedFile, Utils.tokenize(Utils.mkSentence(annotatedSentences)).mkString("\n"))
    }

    IO.remove(passagesFile)
    IO.remove(sentencesFile)
    IO.remove(tokenizedFile)

    IO.list(annotatedDirectory, ".xml").foreach(f ⇒ out(BioC.extractAnnotatedSentences(f, method)))

    val tokenFreqs = BioC.getFrequencies(tokenizedFile)

    IO.write(tokenizedFreqsFile, Utils.stringifyTuple2Sequence(tokenFreqs))

    val positivePassages = IO.read(passagesFile)
    val negativePassagesFiles = IO.listOthers(method, "annotations_passages.txt")

    val tfrf = BioC.tfRf(method, tokenFreqs, positivePassages, negativePassagesFiles).sortBy(_._2).reverse

    IO.write(tfRfTokenizedFile, Utils.stringifyTuple2Sequence(tfrf))

  }

}
