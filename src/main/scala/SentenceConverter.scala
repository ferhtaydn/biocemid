import bioc.{BioCAnnotation, BioCSentence, BioCPassage}
import bioc.util.CopyConverter
import com.typesafe.config.ConfigFactory
import scala.collection.JavaConversions._

class SentenceConverter extends CopyConverter {

  val config = ConfigFactory.load()

  val MI_0018: List[String] = config.getStringList("BioC.PSIMI.0018.Synonyms").toList
  val MI_0019: List[String] = config.getStringList("BioC.PSIMI.0019.Synonyms").toList
  val MI_0096: List[String] = config.getStringList("BioC.PSIMI.0096.Synonyms").toList
  val MI_0416: List[String] = config.getStringList("BioC.PSIMI.0416.Synonyms").toList

  val keywords: Map[String, List[String]] = Map(
    ("0018", MI_0018),
    ("0019", MI_0019),
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
