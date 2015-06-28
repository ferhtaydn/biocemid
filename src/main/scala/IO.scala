import java.io.{File, FileWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}

object IO {

  def write(path: String, txt: String): Unit = {
    Files.write(Paths.get(path), txt.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE)
  }

  def append(path: String, txt: String): Unit = {
    val fw = new FileWriter(path, true)
    fw.write(txt)
    fw.close()
  }

  def list(dirName: String, suffix: String): List[File] = {
    new File(dirName).listFiles.filter(_.isFile).filter(_.getName.endsWith(suffix)).toList
  }

  def listOthers(method: String): List[File] = {
    new File(".").listFiles.filter(_.isFile).filter{ f =>
      f.getName.contains("annotations_words.txt") && !f.getName.contains(method)
    }.toList
  }

  def read(path: String) = scala.io.Source.fromFile(path).getLines()

  def mkSentence(text: String): String = text.split("\\.\\s").mkString(".\n")

  def remove(file: String) = Files.deleteIfExists(Paths.get(file))

  def stringifyTuple2Sequence(seq: Seq[(String, Double)]): String = {
    val sb = new StringBuilder()
    seq.foreach(a => sb.append(s"${a._1} ${a._2}\n"))
    sb.toString()
  }
}
