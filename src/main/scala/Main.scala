
object Main extends App {

  val annotatedDirectory = "annotated_xml"
  val annotationDirectory = "xml/dataset"

  Console.println(

    s"""
      |------------ Welcome to the BioCreative V - Task 6 ------------
      |
      |Please press the key for:
      |
      |1 - To generate the helper information files from training data in $annotatedDirectory
      |
      |2 - To generate the tr-rf results for each method
      |
      |3 - Annotate the raw BioC files in $annotationDirectory
      |
    """.stripMargin

  )

  val selection = scala.io.StdIn.readInt()

  if (selection == 1) {

    Console.println("Please enter the PSIMI code for method. e.g. 0006")

    val method = scala.io.StdIn.readLine()

    val passagesFile = s"MI${method}_annotations_passages.txt"
    val sentencesFile = s"MI${method}_annotations_sentences.txt"
    val wordsFile = s"MI${method}_annotations_words.txt"
    val groupedWordsFile = s"MI${method}_annotations_groupedWords.txt"
    val tfRfFile = s"MI${method}_annotations_tf-rf.txt"

    def prepareOutputFiles(annotatedSentences: String): Unit = {
      IO.append(passagesFile, annotatedSentences)
      IO.append(sentencesFile, Util.mkSentence(annotatedSentences))
      IO.append(wordsFile, Util.mkSentence(annotatedSentences).toLowerCase.split("\\W+").mkString("\n"))
    }

    IO.remove(passagesFile)
    IO.remove(sentencesFile)
    IO.remove(wordsFile)
    IO.remove(groupedWordsFile)

    IO.list(annotatedDirectory, ".xml").foreach(f => prepareOutputFiles(BioC.extractAnnotatedSentences(f, method)))

    IO.write(groupedWordsFile, Util.stringifyTuple2Sequence(BioC.getFrequencies(wordsFile)))

  } else if (selection == 2) {

    Console.println("Please enter the PSIMI code for method. e.g. 0006")

    val method = scala.io.StdIn.readLine()

    val wordsFile = s"MI${method}_annotations_words.txt"
    val tfRfFile = s"MI${method}_annotations_tf-rf.txt"

    IO.write(tfRfFile, Util.stringifyTuple2Sequence(BioC.tfRf(method, wordsFile).sortBy(_._2).reverse))

  } else if (selection == 3) {

    BioC.annotate(annotationDirectory)

  } else {

    Console.println("Please select the options from 1 until 3.")
    System.exit(0)

  }

}
