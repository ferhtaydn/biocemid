package com.ferhtaydn.biocreative5.task1.subtask6

import java.io.{ File, FileWriter }
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, StandardOpenOption }

object IO {

  val annotatedDirectory = "files/manual_annotated_data_set"
  val annotationDirectory = "files/bc5_dataset"
  val algoResultsDirectory = "files/annotated_before_after_results"
  val bc3Word2vecsDirectory = "files/bc3_word2vecs"
  val oaWord2vecsDirectory = "files/oa_word2vecs"
  val bc3Word2vecAnnotationDirectory = "files/bc3_word2vecAnnotation"
  val oaWord2vecAnnotationDirectory = "files/oa_word2vecAnnotation"
  val word2vecAnnotationSuffix = "word2vecs_annotated.xml"
  val xmlSuffix = ".xml"
  val txtSuffix = ".txt"
  val word2vecResultFileSuffix = "result.txt"

  def write(path: String, txt: String): Unit = {
    Files.write(Paths.get(path), txt.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE)
  }

  def append(path: String, txt: String): Unit = {
    val fw = new FileWriter(path, true)
    fw.write(txt)
    fw.close()
  }

  def list(dirName: String, suffix: String): List[File] = {
    val dir = new File(dirName)
    if (dir.exists()) {
      dir.listFiles.filter(_.isFile).filter(_.getName.endsWith(suffix)).toList
    } else {
      Nil
    }
  }

  def listOthers(method: String, suffix: String): List[File] = {
    new File(".").listFiles.filter(_.isFile).filter { f ⇒
      f.getName.contains(suffix) && !f.getName.contains(method)
    }.toList
  }

  def read(path: String): Seq[String] = scala.io.Source.fromFile(path).getLines().toSeq

  def read(file: File): Seq[String] = scala.io.Source.fromFile(file).getLines().toSeq

  def remove(file: String): Boolean = Files.deleteIfExists(Paths.get(file))

}
