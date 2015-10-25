package com.ferhtaydn.biocreative5.task1.subtask6

import java.io.{ File, FileWriter }
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, StandardOpenOption }

import scala.collection.Iterator

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

  def listOthers(method: String, suffix: String): List[File] = {
    new File(".").listFiles.filter(_.isFile).filter { f ⇒
      f.getName.contains(suffix) && !f.getName.contains(method)
    }.toList
  }

  def read(path: String): Seq[String] = scala.io.Source.fromFile(path).getLines().toSeq

  def remove(file: String): Boolean = Files.deleteIfExists(Paths.get(file))

}
