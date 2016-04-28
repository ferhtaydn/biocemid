package com.ferhtaydn.biocemid.annotators.tfrf

import java.io.File

import com.ferhtaydn.biocemid._

import scala.xml.XML

object TfrfHelper {

  def help(): Unit = {
    methodIds.foreach(createHelperFiles)
    methodIds.foreach(createTfrfFile)
  }

  private def createHelperFiles(methodId: String): Unit = {

    val passagesFile = s"MI${methodId}_annotations_passages.txt"
    val sentencesFile = s"MI${methodId}_annotations_sentences.txt"
    val tokenizedFile = s"MI${methodId}_tokenized_words.txt"
    val tokenizedFreqsFile = s"MI${methodId}_tokenized_freqs.txt"

    def out(annotatedSentences: String): Unit = {
      append(passagesFile, annotatedSentences)
      append(sentencesFile, mkSentence(annotatedSentences))
      append(tokenizedFile, tokenize(mkSentence(annotatedSentences)).mkString(newline))
    }

    remove(passagesFile)
    remove(sentencesFile)
    remove(tokenizedFile)

    list(goldResultDirectory, xmlSuffix).foreach(f ⇒ out(extractAnnotatedSentences(f, methodId)))

    write(tokenizedFreqsFile, stringifyTuples(calcFrequenciesFromTokensFile(tokenizedFile)))

  }

  private def createTfrfFile(methodId: String): Unit = {

    val passagesFile = s"MI${methodId}_annotations_passages.txt"
    val tokenizedFile = s"MI${methodId}_tokenized_words.txt"
    val tfRfTokenizedFile = s"MI${methodId}_tokenized_tf-rf.txt"

    val tokenFreqs = calcFrequenciesFromTokensFile(tokenizedFile)

    val positivePassages = read(passagesFile)
    val negativePassagesFiles = listOthers(methodId, "annotations_passages.txt")

    val tfrf = tfRf(tokenFreqs, positivePassages, negativePassagesFiles).sortBy(_._2).reverse

    write(tfRfTokenizedFile, stringifyTuples(tfrf))
  }

  private def calcFrequenciesFromTokensFile(tokensFile: String): Seq[(String, Double)] = {
    calcTokenFrequencies(read(tokensFile))
  }

  private def calcTokenFrequencies(tokens: Seq[String]): Seq[(String, Double)] = {
    val tokenFreqs = tokens.foldLeft(Map.empty[String, Double]) {
      (m, word) ⇒ m + (word → (m.getOrElse(word, 0.0) + 1.0))
    }
    tokenFreqs.toSeq.sortBy(_._2).reverse
  }

  private def extractAnnotatedSentences(file: File, method: String): String = {

    val bioCFile = XML.loadFile(file)
    val passages = bioCFile \\ "passage"
    val annotations = passages \\ "annotation"

    val filtered = annotations.filter(annot ⇒
      (annot \ "infon").filter(i ⇒
        (i \\ "@key").text.equals("PSIMI")).text.equals(method))

    filtered.map(m ⇒ (m \\ "text").text).mkString(newline)

  }

  private def log2(x: Double) = scala.math.log(x) / scala.math.log(2)

  private def tfRf(tokenFreqs: Seq[(String, Double)], positivePassages: Seq[String],
    negativePassagesFiles: List[File]): Seq[(String, Double)] = {

    val positiveCategory = positivePassages.map(tokenize(_).toSet).toList
    val negativeCategory = negativePassagesFiles.flatMap(read(_).map(tokenize(_).toSet))

    tokenFreqs map {

      case (word, freq) ⇒

        val a = positiveCategory.count(_.contains(word))
        val c = negativeCategory.count(_.contains(word))

        val rf = log2(2.0 + (a / scala.math.max(1.0, c)))
        val tfRf = freq * rf

        (word, tfRf)
    }
  }

}
