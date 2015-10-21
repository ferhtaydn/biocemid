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
      |1 - To generate the helper information files from training data in $annotatedDirectory
      |
      |2 - To generate the tr-rf results for each method
      |
      |3 - Annotate the raw BioC files in $annotationDirectory
      |
      |4 - Generate Eval results in $algoResultsDirectory
      |
      |5 - Count of each method annotated in $algoResultsDirectory
      |
    """.stripMargin

  )

  val selection = scala.io.StdIn.readInt()

  if (selection == 1) {

    Console.println("Please enter the PSIMI code for method. e.g. 0006")

    val method = scala.io.StdIn.readLine()

    val passagesFile = s"MI${method}_annotations_passages.txt"
    val sentencesFile = s"MI${method}_annotations_sentences.txt"
    val tokenizedFile = s"MI${method}_tokenized_words.txt"
    val tokenizedFreqsFile = s"MI${method}_tokenized_freqs.txt"

    def prepareOutputFiles(annotatedSentences: String): Unit = {
      IO.append(passagesFile, annotatedSentences)
      IO.append(sentencesFile, Utils.mkSentence(annotatedSentences))
      IO.append(tokenizedFile, Utils.tokenize(Utils.mkSentence(annotatedSentences)).mkString("\n"))
    }

    IO.remove(passagesFile)
    IO.remove(sentencesFile)
    IO.remove(tokenizedFile)

    IO.list(annotatedDirectory, ".xml").foreach(f ⇒ prepareOutputFiles(BioC.extractAnnotatedSentences(f, method)))

    IO.write(tokenizedFreqsFile, Utils.stringifyTuple2Sequence(BioC.getFrequencies(tokenizedFile)))

  } else if (selection == 2) {

    Console.println("Please enter the PSIMI code for method. e.g. 0006")

    val method = scala.io.StdIn.readLine()

    val tfRfTokenizedFile = s"MI${method}_tokenized_tf-rf.txt"

    IO.write(tfRfTokenizedFile, Utils.stringifyTuple2Sequence(BioC.tfRf(method).sortBy(_._2).reverse))

  } else if (selection == 3) {

    BioC.annotate(annotationDirectory)

  } else if (selection == 4) {

    BioC.evaluate(annotatedDirectory, algoResultsDirectory)

  } else if (selection == 5) {

    BioC.countOfMethods(annotatedDirectory)

  } else {

    Console.println("Please select the options from 1 until 5.")
    System.exit(0)

  }

}
