
import scala.xml._
import java.io.File

object Main extends App {

  // "/Users/aydinf/Desktop/annotated_xml
  val directory = args(1)

  // "0006"
  val method = args(2)

  val passagesFile = s"MI${method}_annotations_passages.txt"
  val sentencesFile = s"MI${method}_annotations_sentences.txt"
  val wordsFile = s"MI${method}_annotations_words.txt"
  val groupedWordsFile = s"MI${method}_annotations_groupedWords.txt"

  Util.remove(groupedWordsFile)

  Util.list(directory).foreach(extractAnnotations(_, method))

  Util.write(groupedWordsFile, wordCount(wordsFile))

  def wordCount(wordsFile: String) = {

    val groupedWords = Util.read(wordsFile).foldLeft(Map.empty[String, Int]) {
      (m, word) => m + (word -> (m.getOrElse(word, 0) + 1))
    }

    val sorted = groupedWords.toSeq.sortBy(_._2).reverse

    val sb = new StringBuilder()
    sorted.foreach(a => sb.append(s"${a._1} ${a._2}\n"))
    sb.toString()

  }

  def extractAnnotations(file: File, method: String): Unit = {

    val bioCFile = XML.loadFile(file)

    val passages = bioCFile \\ "passage"

    val annotations = passages \\ "annotation"

    val result = annotations.filter(annot =>
      (annot \ "infon").filter(i =>
        (i \\ "@key").text.equals("PSIMI")
      ).text.equals(method)
    ).map(m => (m \\ "text").text).mkString("\n")

    Util.append(passagesFile, result)

    Util.append(sentencesFile, Util.mkSentence(result))

    Util.append(wordsFile, Util.mkSentence(result).toLowerCase.split("\\W+").mkString("\n"))

  }

}
