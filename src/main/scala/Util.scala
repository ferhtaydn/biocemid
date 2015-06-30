
object Util {

  def extractFileName(fileName: String, suffix: String) = fileName.split(suffix).head

  def stringifyTuple2Sequence(seq: Seq[(String, Double)]): String = {
    val sb = new StringBuilder()
    seq.foreach(a => sb.append(s"${a._1} ${a._2}\n"))
    sb.toString()
  }

  def mkSentence(text: String): String = text.split("\\.\\s").mkString(".\n")

  def mkSentenceList(text: String): List[String] = text.split("\\.\\s").toList.map { s =>
    val sentence = s.trim
    if (!sentence.last.equals('.')) sentence.concat(".") else sentence
  }

  def mkWordList(sentence: String): List[String] = sentence.toLowerCase.split("\\s").toList

}
