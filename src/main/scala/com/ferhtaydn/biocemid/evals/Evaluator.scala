package com.ferhtaydn.biocemid.evals

import bioc.BioCAnnotation
import com.ferhtaydn.biocemid._

import scala.collection.JavaConversions._
import scala.collection.mutable

//noinspection ScalaStyle
object Evaluator {

  def countOfMethods(dir: String, suffix: String): Unit = {

    val methodCountWithinPassages = mutable.Map.empty[String, Int].withDefaultValue(0)
    val totalMethodCountWithinPassages = mutable.Map.empty[String, Int].withDefaultValue(0)
    val methodCountWithinArticles = mutable.Map.empty[String, Set[String]].withDefaultValue(Set.empty[String])
    val textsOfMethods = mutable.Map.empty[String, List[String]].withDefaultValue(List.empty[String])

    list(dir, suffix).foreach { file ⇒

      getBioCPassages(file).foreach { passage ⇒
        passage.getAnnotations.toList.groupBy(a ⇒ a.getInfon(psimi)).foreach {
          case (id, c) ⇒
            methodCountWithinPassages.update(id, methodCountWithinPassages(id) + 1)
            methodCountWithinArticles.update(id, methodCountWithinArticles(id) + file.getName)
            totalMethodCountWithinPassages.update(id, totalMethodCountWithinPassages(id) + c.length)
            textsOfMethods.update(id, textsOfMethods(id) ++ c.map(_.getText))
        }
      }
    }

    methodIds.foreach { id ⇒

      Console.println(
        s"""
           |---------------------------------------------
           |
           |MI:$id
           |In ${methodCountWithinArticles(id).size} article(s): ${methodCountWithinArticles(id)}
           |In ${methodCountWithinPassages(id)} passage(s)
           |Total: ${totalMethodCountWithinPassages(id)}
           |Texts:
         """.stripMargin
      )

      textsOfMethods(id).foreach { t ⇒
        Console.println(t)
        Console.println()
      }
    }
  }

  def countOfMethods2(dir: String, suffix: String): Unit = {

    val tn = list(dir, suffix).flatMap { file ⇒
      getBioCPassages(file).filter(p ⇒ p.getAnnotations.isEmpty)
    }

    println(tn.length)

    val annotatedParagraphs = list(dir, suffix).flatMap { file ⇒
      getBioCPassages(file).filter(p ⇒ p.getAnnotations.nonEmpty)
    }

    println(annotatedParagraphs.length)

    val annotations = list(dir, suffix).flatMap { file ⇒
      getBioCPassages(file).flatMap(p ⇒ p.getAnnotations)
    }

    println(annotations.length)

  }

  def evaluate(manuallyAnnotatedFilesDirectory: String, annotatedFilesDirectory: String, fileSuffix: String): Unit = {

    val manuallyAnnotatedFiles = list(manuallyAnnotatedFilesDirectory, fileSuffix)
    val annotatedFiles = list(annotatedFilesDirectory, fileSuffix)

    remove(s"$annotatedFilesDirectory/FN_annotations.txt")
    remove(s"$annotatedFilesDirectory/FP_annotations.txt")

    val results = scala.collection.mutable.Map.empty[Rate, Double].withDefaultValue(0)

    manuallyAnnotatedFiles.zip(annotatedFiles).foreach {
      case (manuallyAnnotatedFile, annotatedFile) ⇒

        val fileRate = FileRate(manuallyAnnotatedFile.getName)

        val manuallyAnnotatedPassages = getBioCPassages(manuallyAnnotatedFile)
        val annotatedPassages = getBioCPassages(annotatedFile)

        manuallyAnnotatedPassages.zip(annotatedPassages).foreach {
          case (manuallyAnnotatedPassage, annotatedPassage) ⇒

            //TODO: take separate run results with that line.
            //combineCoImmunoPrecipitations(manuallyAnnotatedPassage)

            val manualAnnotations = manuallyAnnotatedPassage.getAnnotations.toList
            val annotations = annotatedPassage.getAnnotations.toList

            matchAnnotations(manualAnnotations, annotations, fileRate, annotatedFilesDirectory)
        }
        calculateFileResults(results, fileRate)
    }

    calculateTotalResults(results.toMap)

  }

  private def matchAnnotations(manualAnnotations: List[BioCAnnotation], annotations: List[BioCAnnotation],
    fileRate: FileRate, annotatedFilesDirectory: String) = {

    if (manualAnnotations.isEmpty && annotations.isEmpty) {
      fileRate.incTN(1)
    } else {
      // manualAnnotations.nonEmpty && annotations.nonEmpty
      val consumedAnnotations = manualAnnotations.foldLeft(List.empty[BioCAnnotation]) {
        case (acc, manualAnnotation) ⇒
          val manualAnnotationText = manualAnnotation.getText
          annotations.filter { annotation ⇒
            annotation.getInfon(psimi).equalsIgnoreCase(manualAnnotation.getInfon(psimi)) &&
              matchesLocations(manualAnnotation, annotation)
          } match {
            case Nil ⇒
              fileRate.incFN(1)
              append(s"$annotatedFilesDirectory/FN_annotations.txt", manualAnnotation.getInfon(psimi) + newline + manualAnnotation.getText + newline)
              acc
            case matchedAnnotations ⇒
              // big manual annotated text with multi small code annotated text.
              val (matchingTextLength, annotationTextLength) = matchedAnnotations.foldLeft((0d, 0d)) {
                case ((mtl, atl), annotation) ⇒
                  val annotationText = annotation.getText
                  val matchingText = getCommonText(manualAnnotationText, annotationText)
                  (mtl + matchingText.length.toDouble, atl + annotationText.length.toDouble)
              }

              val unionLength = manualAnnotationText.length + annotationTextLength - matchingTextLength

              // tpScore + fnScore + fpScore = 1.0
              val tpScore = matchingTextLength / unionLength
              val fpScore = (annotationTextLength - matchingTextLength) / unionLength
              val fnScore = (manualAnnotationText.length - matchingTextLength) / unionLength

              fileRate.incTP(tpScore)
              fileRate.incFP(fpScore)
              fileRate.incFN(fnScore)

              acc ++ matchedAnnotations
          }
      }

      fileRate.incFP((annotations diff consumedAnnotations).length)
      append(s"$annotatedFilesDirectory/FP_annotations.txt", (annotations diff consumedAnnotations).map(x ⇒ x.getInfon(psimi) + newline + x.getText + newline).mkString(newline))

    }
  }

}
