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

  private var annotationId: Int = -1

  private def psiMiDecider(words: List[String]): Option[String] = {

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

  private def annotateSentence(sentence: BioCSentence): Option[BioCAnnotation] = {

    psiMiDecider(Util.mkWordList(sentence.getText)) match {
      case None => None
      case Some(psimiInfon) =>

        val annotationInfons = Map("type" -> "ExperimentalMethod", "PSIMI" -> psimiInfon)
        val out: BioCAnnotation = new BioCAnnotation
        out.setInfons(annotationInfons)
        out.setText(sentence.getText)
        annotationId += 1
        out.setID(annotationId.toString)
        out.setLocation(sentence.getOffset, sentence.getText.length)
        Option(out)
    }
  }

  private def sentenceWithOffset(sentences: List[String]) = {

    def loop(sentences: List[String], count: Int, acc: Seq[(String, Int, Int)]): Seq[(String, Int, Int)] = sentences match {
      case Nil => acc
      case x::xs => loop(xs, x.length + count + 1, acc :+ (x, count, x.length))
    }

    loop(sentences, 0, Seq.empty[(String, Int, Int)])
  }

  override def getPassage(in: BioCPassage): BioCPassage = {

    val out: BioCPassage = new BioCPassage
    out.setOffset(in.getOffset)
    out.setInfons(in.getInfons)
    out.setText(in.getText)

    if (!in.getInfons.get("type").contains("title")) {

      val sentences: List[String] = Util.mkSentenceList(in.getText)

      sentenceWithOffset(sentences).foreach { case (s, o, l) =>

        val sentence: BioCSentence = new BioCSentence
        sentence.setOffset(in.getOffset + o)
        sentence.setText(s)

        annotateSentence(sentence) match {
          case None =>
          case Some(annotation) =>
            out.addAnnotation(annotation)
        }
      }
    }

    out
  }

}
