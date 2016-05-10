package com.ferhtaydn.biocemid.evals

case class FileRate(fileName: String) {

  private var falseNegatives: Double = 0d
  private var falsePositives: Double = 0d
  private var trueNegatives: Double = 0d
  private var truePositives: Double = 0d

  def incFN(i: Double): Unit = { falseNegatives += i }
  def fn: Double = falseNegatives

  def incFP(i: Double): Unit = { falsePositives += i }
  def fp: Double = falsePositives

  def incTN(i: Double): Unit = { trueNegatives += i }
  def tn: Double = trueNegatives

  def incTP(i: Double): Unit = { truePositives += i }
  def tp: Double = truePositives

  override def toString: String = {
    s"""
       |$fileName
       |falseNegatives: $fn
       |falsePositives: $fp
       |trueNegatives: $tn
       |truePositives: $tp
      """.stripMargin
  }

}
