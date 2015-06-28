
object Main extends App {

  val directory = "annotated_xml"

  // "0006"
  val method = args(1)

  val passagesFile = s"MI${method}_annotations_passages.txt"
  val sentencesFile = s"MI${method}_annotations_sentences.txt"
  val wordsFile = s"MI${method}_annotations_words.txt"
  val groupedWordsFile = s"MI${method}_annotations_groupedWords.txt"
  val tfRfFile = s"MI${method}_annotations_tf-rf.txt"

  if (args.length == 2) {
    IO.remove(passagesFile)
    IO.remove(sentencesFile)
    IO.remove(wordsFile)
    IO.remove(groupedWordsFile)

    IO.list(directory, ".xml").foreach(f => prepareOutputFiles(BioC.extractAnnotatedSentences(f, method)))

    IO.write(groupedWordsFile, IO.stringifyTuple2Sequence(BioC.getFrequencies(wordsFile)))
  }

  if (args.length > 2 && args(2).equals("tfrf")) {
    IO.write(tfRfFile, IO.stringifyTuple2Sequence(tfRf.sortBy(_._2).reverse))
  }

  def prepareOutputFiles(annotatedSentences: String): Unit = {
    IO.append(passagesFile, annotatedSentences)
    IO.append(sentencesFile, IO.mkSentence(annotatedSentences))
    IO.append(wordsFile, IO.mkSentence(annotatedSentences).toLowerCase.split("\\W+").mkString("\n"))
  }

  def tfRf: Seq[(String, Double)] = {

    val methodFreqs = BioC.getFrequencies(wordsFile)
    val otherFiles = IO.listOthers(method).map(f => f.getPath)

    methodFreqs.map { case (word, freq) =>

      def log2(x: Double) = scala.math.log(x) / scala.math.log(2)

      val otherTotal = otherFiles.map(BioC.getFrequencies(_).find(wf => word.equals(wf._1)).fold(0.0)(_._2)).sum

      val rf = log2(2.0 + (freq / scala.math.max(1.0, otherTotal)))
      val tfRf = freq * rf

      //println(s"$word $freq $otherTotal $rf $tfRf")

      (word, tfRf)
    }
  }

}
