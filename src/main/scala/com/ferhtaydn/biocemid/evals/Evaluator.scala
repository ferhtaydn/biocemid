package com.ferhtaydn.biocemid.evals

import bioc.BioCAnnotation
import com.ferhtaydn.biocemid._

import scala.collection.JavaConversions._
import scala.collection.mutable

//noinspection ScalaStyle
object Evaluator {

  def countOfMethods(dir: String, suffix: String): Unit = {

    val methodCountWithinPassages = mutable.Map.empty[String, Int].withDefaultValue(0)
    val methodCountWithinArticles = mutable.Map.empty[String, Set[String]].withDefaultValue(Set.empty[String])

    list(dir, suffix).foreach { file ⇒

      getBioCPassages(file).foreach { passage ⇒
        passage.getAnnotations.toList.groupBy(a ⇒ a.getInfon(psimi)).map(x ⇒ x._1 → x._2.size).foreach {
          case (n, c) ⇒
            methodCountWithinPassages.update(n, methodCountWithinPassages(n) + c)
            methodCountWithinArticles.update(n, methodCountWithinArticles(n) + file.getName)
        }
      }
    }

    methodIds.foreach { id ⇒
      Console.println(
        s"""
           |MI:$id
           |In ${methodCountWithinArticles(id).size} articles: ${methodCountWithinArticles(id)}
           |In ${methodCountWithinPassages(id)} passages
         """.stripMargin
      )
    }
  }

  def evaluate(manuallyAnnotatedFilesDirectory: String, annotatedFilesDirectory: String, fileSuffix: String): Unit = {

    val manuallyAnnotatedFiles = list(manuallyAnnotatedFilesDirectory, fileSuffix)
    val annotatedFiles = list(annotatedFilesDirectory, fileSuffix)

    val results = scala.collection.mutable.Map.empty[Rate, Double].withDefaultValue(0)

    manuallyAnnotatedFiles.zip(annotatedFiles).foreach {
      case (manuallyAnnotatedFile, annotatedFile) ⇒

        val fileRate = FileRate(manuallyAnnotatedFile.getName)

        val manuallyAnnotatedPassages = getBioCPassages(manuallyAnnotatedFile)
        val annotatedPassages = getBioCPassages(annotatedFile)

        manuallyAnnotatedPassages.zip(annotatedPassages).foreach {
          case (manuallyAnnotatedPassage, annotatedPassage) ⇒

            combineCoImmunoPrecipitations(manuallyAnnotatedPassage)

            val manualAnnotations = manuallyAnnotatedPassage.getAnnotations.toList
            val annotations = annotatedPassage.getAnnotations.toList

            matchAnnotations(manualAnnotations, annotations, fileRate)
        }
        calculateFileResults(results, fileRate)
    }

    calculateTotalResults(results.toMap)

  }

  private def matchAnnotations(manualAnnotations: List[BioCAnnotation], annotations: List[BioCAnnotation],
    fileRate: FileRate) = {

    if (manualAnnotations.isEmpty && annotations.isEmpty) {
      fileRate.incTN(1)
    } else if (manualAnnotations.isEmpty && annotations.nonEmpty) {
      fileRate.incFP(annotations.length)
    } else if (manualAnnotations.nonEmpty && annotations.isEmpty) {
      fileRate.incFN(manualAnnotations.length)
    } else {
      // manualAnnotations.nonEmpty && annotations.nonEmpty
      val consumedAnnotations = manualAnnotations.foldLeft(List.empty[BioCAnnotation]) {
        case (acc, manualAnnotation) ⇒
          annotations.filter { annotation ⇒
            annotation.getInfon(psimi).equalsIgnoreCase(manualAnnotation.getInfon(psimi)) &&
              matchesLocations(manualAnnotation, annotation)
          } match {
            case Nil ⇒
              fileRate.incFN(1)
              acc
            case matchedAnnotations ⇒
              // big manual annotated text with multi small code annotated text.
              matchedAnnotations.foreach(a ⇒ fileRate.incTP(calculateTP(manualAnnotation.getText, a.getText)))
              acc ++ matchedAnnotations
          }
      }

      fileRate.incFP((annotations diff consumedAnnotations).length)

    }
  }

}
