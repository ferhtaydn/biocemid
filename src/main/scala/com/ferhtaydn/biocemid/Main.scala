package com.ferhtaydn.biocemid

import com.ferhtaydn.biocemid.annotators.baseline.BaselineAnnotatorConfig
import com.ferhtaydn.biocemid.annotators.tfrf.{ TfrfAnnotatorConfig, TfrfHelper }
import com.ferhtaydn.biocemid.annotators.word2vec.{ Word2vecAnnotatorConfig, Word2vecHelper }
import com.ferhtaydn.biocemid.evals.Evaluator

//noinspection ScalaStyle
object Main extends App {

  Console.println(
    s"""
       |------------ Welcome to the BioCreative V - BioC Task - Subtask 6 ------------
       |
       |Please press the key for:
       |1 - To generate the helper information files and tf-rf results from labelled data in $goldResultDirectory
       |2 - Annotate with BASELINE raw files in $rawDirectory
       |3 - Annotate with TFRF raw files in $rawDirectory
       |4 - Annotate with WORD2VEC over PURE-BASELINE, raw files in $rawDirectory
       |5 - Annotate with WORD2VEC + GENIATAGGER, raw files in $rawDirectory
       |6 - Annotate with WORD2VEC + GENIATAGGER + INO, raw files in $rawDirectory
       |7 - Generate Eval results by comparing $goldResultDirectory and $baselineResultDirectory
       |8 - Generate Eval results by comparing $goldResultDirectory and $tfrfResultDirectory
       |9 - Generate Eval results by comparing $goldResultDirectory and $word2vecResultDirectory
       |10 - Count of each method annotated in $publishedDataSet
     """.stripMargin
  )

  scala.io.StdIn.readInt() match {
    case 1 ⇒
      TfrfHelper.help()
    case 2 ⇒
      val config = BaselineAnnotatorConfig(rawDirectory, 1, 1d, 0.5, baselineAnnotatedSuffix, pureBaseline = true)
      annotators.annotate(config)
    case 3 ⇒
      val config = TfrfAnnotatorConfig(rawDirectory, 1, 1d, 0.5, tfrfAnnotatedSuffix, pureBaseline = true,
        useINO = true, useNamedEntity = true)
      annotators.annotate(config)
    case 4 ⇒
      val config = Word2vecAnnotatorConfig(oaWord2vecsPureBaselineDirectory, word2vecResultDedupeFileSuffix, rawDirectory,
        1, 0.9, 0.7, word2vecAnnotatedSuffix, pureBaseline = true, useNamedEntity = true, useINO = true)
      Word2vecHelper.help(config)
      annotators.annotate(config)
    case 5 ⇒
      val config = Word2vecAnnotatorConfig(oaWord2vecsDirectory, word2vecResultDedupeFileSuffix, rawDirectory,
        1, 1d, 0.5, word2vecAnnotatedSuffix, useNamedEntity = true)
      Word2vecHelper.help(config)
      annotators.annotate(config)
    case 6 ⇒
      val config = Word2vecAnnotatorConfig(oaWord2vecsDirectory, word2vecResultDedupeFileSuffix,
        rawDirectory, 1, 1d, 0.5, word2vecAnnotatedSuffix, useNamedEntity = false, useINO = true)
      Word2vecHelper.help(config)
      annotators.annotate(config)
    case 7 ⇒
      Evaluator.evaluate(goldResultDirectory, pureBaselineResultDirectory, xmlSuffix)
    case 8 ⇒
      Evaluator.evaluate(goldResultDirectory, tfrfResultDirectory, xmlSuffix)
    case 9 ⇒
      Evaluator.evaluate(goldResultDirectory, word2vecResultDirectory, xmlSuffix)
    case 10 ⇒
      Evaluator.countOfMethods(publishedDataSet, xmlSuffix)
    case _ ⇒
      Console.println("Please select the options from 1 to 10.")
      System.exit(0)
  }
}
