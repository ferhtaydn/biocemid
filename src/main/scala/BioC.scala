
import java.io.File
import scala.xml.XML

object BioC {

  def getFrequencies(wordsFile: String): Seq[(String, Double)] = {
    val groupedWords = IO.read(wordsFile).foldLeft(Map.empty[String, Double]) {
      (m, word) => m + (word -> (m.getOrElse(word, 0.0) + 1.0))
    }
    groupedWords.toSeq.sortBy(_._2).reverse
  }

  def extractAnnotatedSentences(file: File, method: String): String = {

    val bioCFile = XML.loadFile(file)
    val passages = bioCFile \\ "passage"
    val annotations = passages \\ "annotation"

    val filtered = annotations.filter(annot =>
      (annot \ "infon").filter(i =>
        (i \\ "@key").text.equals("PSIMI")
      ).text.equals(method)
    )

    filtered.map(m => (m \\ "text").text).mkString("\n")
  }
}
