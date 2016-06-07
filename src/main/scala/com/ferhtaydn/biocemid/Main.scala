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
       |2 - Annotate with BASELINE raw files in $manualAnnotationRawDirectory
       |3 - Annotate with TFRF raw files in $manualAnnotationRawDirectory
       |4 - Annotate with WORD2VEC, raw files in $manualAnnotationRawDirectory
       |5 - Annotate with WORD2VEC + GENIATAGGER, raw files in $manualAnnotationRawDirectory
       |6 - Annotate with WORD2VEC + GENIATAGGER + INO, raw files in $manualAnnotationRawDirectory
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
      val config = BaselineAnnotatorConfig(1, 1d, 0.5, baselineAnnotatedSuffix, pureBaseline = true)
      annotators.annotate(manualAnnotationRawDirectory, config)
    case 3 ⇒
      val config = TfrfAnnotatorConfig(1, 1d, 0.5, tfrfAnnotatedSuffix)
      annotators.annotate(manualAnnotationRawDirectory, config)
    case 4 ⇒
      val config = Word2vecAnnotatorConfig(oaWord2vecsDirectory, word2vecResultFileSuffix,
        1, 1d, 0.5, word2vecAnnotatedSuffix, pureBaseline = true)
      Word2vecHelper.help(config)
      annotators.annotate(manualAnnotationRawDirectory, config)
    case 5 ⇒
      val config = Word2vecAnnotatorConfig(oaWord2vecsDirectory, word2vecResultDedupeFileSuffix, 1, 1d, 0.5,
        word2vecAnnotatedSuffix, useNamedEntity = true)
      Word2vecHelper.help(config)
      annotators.annotate(manualAnnotationRawDirectory, config)
    case 6 ⇒
      val config = Word2vecAnnotatorConfig(oaWord2vecsDirectory, word2vecResultDedupeFileSuffix, 1, 1d, 0.5,
        word2vecAnnotatedSuffix, useNamedEntity = false, useINO = true)
      Word2vecHelper.help(config)
      annotators.annotate(manualAnnotationRawDirectory, config)
    case 7 ⇒
      Evaluator.evaluate(goldResultDirectory, pureBaselineResultDirectory, xmlSuffix)
    case 8 ⇒
      Evaluator.evaluate(goldResultDirectory, tfrfResultDirectory, xmlSuffix)
    case 9 ⇒
      Evaluator.evaluate(goldResultDirectory, word2vecResultDirectory, xmlSuffix)
    case 10 ⇒
      Evaluator.countOfMethods(publishedDataSet, xmlSuffix)
    case _ ⇒
      Console.println("Please select the options from 1 to 9.")
      System.exit(0)
  }
}
