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
       |6 - Generate Eval results by comparing $goldResultDirectory and $baselineResultDirectory
       |7 - Generate Eval results by comparing $goldResultDirectory and $tfrfResultDirectory
       |8 - Generate Eval results by comparing $goldResultDirectory and $word2vecResultDirectory
       |9 - Count of each method annotated in $publishedDataSet
     """.stripMargin
  )

  scala.io.StdIn.readInt() match {
    case 1 ⇒
      TfrfHelper.help()
    case 2 ⇒
      annotators.annotate(manualAnnotationRawDirectory, BaselineAnnotatorConfig(1, 1d, 0.5, baselineAnnotatedSuffix))
    case 3 ⇒
      annotators.annotate(manualAnnotationRawDirectory, TfrfAnnotatorConfig(1, 1d, 0.5, tfrfAnnotatedSuffix))
    case 4 ⇒
      Word2vecHelper.help()
      annotators.annotate(
        manualAnnotationRawDirectory,
        Word2vecAnnotatorConfig(oaWord2vecsDirectory, word2vecResultFileSuffix, 1, 1d, 0.5, word2vecAnnotatedSuffix)
      )
    case 5 ⇒
      Word2vecHelper.help()
      annotators.annotate(
        manualAnnotationRawDirectory,
        Word2vecAnnotatorConfig(oaWord2vecsDirectory, word2vecResultDedupeFileSuffix,
          1, 1d, 0.5, word2vecAnnotatedSuffix,
          useNamedEntity = true)
      )
    case 6 ⇒
      Evaluator.evaluate(goldResultDirectory, pureBaselineResultDirectory, xmlSuffix)
    case 7 ⇒
      Evaluator.evaluate(goldResultDirectory, tfrfResultDirectory, xmlSuffix)
    case 8 ⇒
      Evaluator.evaluate(goldResultDirectory, word2vecResultDirectory, xmlSuffix)
    case 9 ⇒
      Evaluator.countOfMethods(publishedDataSet, xmlSuffix)
    case _ ⇒
      Console.println("Please select the options from 1 to 9.")
      System.exit(0)
  }
}
