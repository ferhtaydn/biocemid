import java.io.{File, FileWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}

object Util {

  def write(path: String, txt: String): Unit = {
    Files.write(Paths.get(path), txt.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE)
  }

  def append(path: String, txt: String): Unit = {
    val fw = new FileWriter(path, true)
    fw.write(txt)
    fw.close()
  }

  def list(dirName: String): List[File] = {
    new File(dirName).listFiles.filter(_.isFile).filter(_.getName.endsWith(".xml")).toList
  }

  def read(path: String) = scala.io.Source.fromFile(path).getLines

  def mkSentence(text: String): String = text.split("\\.\\s").mkString(".\n")

  def remove(file: String) = Files.deleteIfExists(Paths.get(file))
}
