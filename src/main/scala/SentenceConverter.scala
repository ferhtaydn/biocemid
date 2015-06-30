import bioc.{BioCAnnotation, BioCSentence, BioCPassage}
import bioc.util.CopyConverter
import scala.collection.JavaConversions._

class SentenceConverter extends CopyConverter {

  val MI_0416: List[String] = List(
    "microscopy", "fluorescence", "immunofluorescence",
    "gfp", "rfp", "infected", "image", "imaging", "get1", "subcellular", "conjugated"
  )
  val MI_0096: List[String] = List(
    "pull", "down", "pull-down", "pull down", "pulldown", "affinity", "affinity capture",
    "gst", "appl1", "rab5", "gel", "glutathione"
  )
  val MI_0018: List[String] = List(
    "yeast", "hybrid", "two-hybrid", "two hybrid", "y2h","y-2h",
    "2 hybrid", "2-hybrid", "2h", "classical two hybrid",
    "Gal4 transcription regeneration", "yeast two hybrid",
    "bait", "cdna", "gal4", "gal", "galactosidase"
  )

  val MI_0006_0007_as_0019: List[String] = List(
    "co-immunoprecipitation", "coimmunoprecipitation", "co-ip", "coip",
    "immunoprecipitation", "immunoprecipitated", "immunoprecipitates",
    "anti bait coip", "anti tag coip", "precipitated", "precipitate", "precipitates",
    "antibody", "antibodies", "tag", "tagged", "bait"
  )

  val keywords: Map[String, List[String]] = Map(
    ("0019", MI_0006_0007_as_0019),
    ("0018", MI_0018),
    ("0096", MI_0096),
    ("0416", MI_0416)
  )

  var annotationId: Int = -1

  def psiMiDecider(words: List[String]): Option[String] = {

    val result = keywords.map { case (method, ks) =>
      val result = words.flatMap { w =>
        ks.filter(k => k.equalsIgnoreCase(w))
      }.size
      (method, result)
    }.toSeq.sortBy(_._2)

    result.last match { case (m, c) =>
      if (c > 0) Some(m) else None
    }
  }

  def annotateSentence(sentence: String): Option[BioCAnnotation] = {

    psiMiDecider(sentence.toLowerCase.split("\\s").toList) match {
      case None => None
      case Some(psimiInfon) =>

        val annotationInfons = Map("type" -> "ExperimentalMethod", "PSIMI" -> psimiInfon)
        val out: BioCAnnotation = new BioCAnnotation
        out.setInfons(annotationInfons)
        out.setText(sentence)
        annotationId += 1
        out.setID(annotationId.toString)
        Option(out)
    }
  }

  override def getPassage(in: BioCPassage): BioCPassage = {

    val out: BioCPassage = new BioCPassage
    out.setOffset(in.getOffset)
    out.setInfons(in.getInfons)
    out.setText(in.getText)

    val text: String = in.getText
    var current: Int = 0
    var period: Int = text.indexOf(". ", current)

    while (period > -1) {

      val sentence: BioCSentence = new BioCSentence
      sentence.setOffset(out.getOffset + current)
      sentence.setText(text.substring(current, period + 1))
      //out.addSentence(sentence)

      annotateSentence(sentence.getText) match {
        case None =>
        case Some(annotation) =>
          out.addAnnotation(annotation)
      }

      current = period + 2
      while (current < text.length && text.charAt(current) == ' ') {
        current += 1  // skip extra spaces
      }

      if (current >= text.length) {
        current = -1
        period = -1
      } else {
        period = text.indexOf(". ", current)
      }
    }

    /*
    if (current > -1) {
      val sentence: BioCSentence = new BioCSentence
      sentence.setOffset(out.getOffset + current)
      sentence.setText(text.substring(current))
      out.addSentence(sentence)
    }
    */

    out
  }

}
