package com.ferhtaydn.biocreative5.task1.subtask6

import java.io.{ File, FileWriter }
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, StandardOpenOption }

object IO {

  // gold_set_13, gold_set_17 and gold_set_30 files contains the articles from $manualAnnotationStatistics
  val goldResultDirectory = "files/gold_set_30"
  val manualAnnotationStatistics = "files/manually_annotated_data_set_by_2_annotator"

  val word2vecResultDirectory = "files/results/word2vec/config_2_2_30_articles"
  val tfrfResultDirectory = "files/results/tfrf/manual/tfrf_30_articles"
  val baselineResultDirectory = "files/results/baseline_30_articles"

  val manualAnnotationRawDirectory = "files/manual_annotation_raw_30"

  val oaWord2vecsDirectory = "files/oa_word2vecs"

  val xmlSuffix = ".xml"
  val txtSuffix = ".txt"
  val word2vecResultFileSuffix = "result.txt"
  val word2vecResultDedupeFileSuffix = "result_dedupe.txt"
  val baselineResultSuffix = "baseline.xml"
  val tfrfResultSuffix = "tfrf.xml"
  val word2vecAnnotationSuffix = "word2vec.xml"

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
