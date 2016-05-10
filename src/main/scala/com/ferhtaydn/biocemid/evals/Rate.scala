package com.ferhtaydn.biocemid.evals

sealed trait Rate
case object FN extends Rate
case object TN extends Rate
case object FP extends Rate
case object TP extends Rate
