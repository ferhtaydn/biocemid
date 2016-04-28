package com.ferhtaydn.biocemid

import com.ferhtaydn.biocemid.annotators.Annotator
import com.ferhtaydn.biocemid.annotators.baseline.BaselineAnnotatorConfig
import com.ferhtaydn.biocemid.annotators.tfrf.{ TfrfAnnotatorConfig, TfrfHelper }
import com.ferhtaydn.biocemid.annotators.word2vec.{ Word2vecAnnotatorConfig, Word2vecHelper }

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
       |5 - Generate Eval results by comparing $tfrfResultDirectory and $goldResultDirectory
       |6 - Count of each method annotated in $manualAnnotationStatistics
     """.stripMargin
  )

  scala.io.StdIn.readInt() match {
    case 1 ⇒
      TfrfHelper.help()
    case 2 ⇒
      Annotator.annotate(manualAnnotationRawDirectory, BaselineAnnotatorConfig(1, 0.5, 0.25, baselineAnnotatedSuffix))
    case 3 ⇒
      Annotator.annotate(manualAnnotationRawDirectory, TfrfAnnotatorConfig(1, 0.5, 0.25, tfrfAnnotatedSuffix))
    case 4 ⇒
      Word2vecHelper.help()
      Annotator.annotate(
        manualAnnotationRawDirectory,
        Word2vecAnnotatorConfig(oaWord2vecsDirectory, word2vecResultFileSuffix, 1, 1d, 0.5, word2vecAnnotatedSuffix)
      )
    case 5 ⇒
      Evaluator.evaluate(goldResultDirectory, word2vecResultDirectory, xmlSuffix)
    case 6 ⇒
      Evaluator.countOfMethods(manualAnnotationStatistics, xmlSuffix)
    case _ ⇒
      Console.println("Please select the options from 1 to 6.")
      System.exit(0)
  }
}
