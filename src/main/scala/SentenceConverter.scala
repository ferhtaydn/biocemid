import bioc.{BioCLocation, BioCAnnotation, BioCSentence, BioCPassage}
import bioc.util.CopyConverter
import com.typesafe.config.ConfigFactory
import scala.collection.JavaConversions._

class SentenceConverter extends CopyConverter {

  private var annotationId: Int = -1

  val config = ConfigFactory.load()

  val MI_0018_Synonyms: List[String] = config.getStringList("BioC.PSIMI.0018.Synonyms").toList
  val MI_0019_Synonyms: List[String] = config.getStringList("BioC.PSIMI.0019.Synonyms").toList
  val MI_0096_Synonyms: List[String] = config.getStringList("BioC.PSIMI.0096.Synonyms").toList
  val MI_0416_Synonyms: List[String] = config.getStringList("BioC.PSIMI.0416.Synonyms").toList

  val MI_0018_Keywords: List[String] = config.getStringList("BioC.PSIMI.0018.Keywords").toList
  val MI_0019_Keywords: List[String] = config.getStringList("BioC.PSIMI.0019.Keywords").toList
  val MI_0096_Keywords: List[String] = config.getStringList("BioC.PSIMI.0096.Keywords").toList
  val MI_0416_Keywords: List[String] = config.getStringList("BioC.PSIMI.0416.Keywords").toList

  val MI_0018_Others: List[String] = config.getStringList("BioC.PSIMI.0018.Others").toList
  val MI_0019_Others: List[String] = config.getStringList("BioC.PSIMI.0019.Others").toList
  val MI_0096_Others: List[String] = config.getStringList("BioC.PSIMI.0096.Others").toList
  val MI_0416_Others: List[String] = config.getStringList("BioC.PSIMI.0416.Others").toList

  val vocabularies: Map[String, (List[String], List[String], List[String])] = Map(
    ("0018", (MI_0018_Synonyms, MI_0018_Keywords, MI_0018_Others)),
    ("0019", (MI_0019_Synonyms, MI_0019_Keywords, MI_0019_Others)),
    ("0096", (MI_0096_Synonyms, MI_0096_Keywords, MI_0096_Others)),
    ("0416", (MI_0416_Synonyms, MI_0416_Keywords, MI_0416_Others))
  )

  override def getPassage(in: BioCPassage): BioCPassage = {

    def annotateSentence(sentence: BioCSentence): Option[BioCAnnotation] = {

      def psiMiDeciderOnVocabularies(words: List[String]): Option[String] = {

        val result = vocabularies.map { case (method, (ss, ks, os)) =>

          val synonym = words.flatMap { w =>
            ss.filter(s => s.equalsIgnoreCase(w))
          }.size

          val keyword = words.flatMap { w =>
            ks.filter(k => k.equalsIgnoreCase(w))
          }.size

          val other = words.flatMap { w =>
            os.filter(o => o.equalsIgnoreCase(w))
          }.size

          (method, (0.5 * synonym) + (0.3 * keyword) + (0.2 * other))

        }.toSeq.sortBy(_._2)

        result.last match {
          case (m, c) =>
            if (c > 0) Some(m) else None
        }
      }

      def psiMiDeciderOnSynonyms(words: List[String]): Option[String] = {

        val result = vocabularies.map { case (method, (ss, _, _)) =>
          val result = words.flatMap { w =>
            ss.filter(s => s.equalsIgnoreCase(w))
          }.size
          (method, result)
        }.toSeq.sortBy(_._2)

        result.last match {
          case (m, c) =>
            if (c > 0) Some(m) else None
        }
      }

      psiMiDeciderOnVocabularies(Util.mkWordList(sentence.getText)) match {
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

    def sentenceWithOffset(sentences: List[String]): Seq[(String, Int, Int)] = {

      def loop(sentences: List[String], count: Int, acc: Seq[(String, Int, Int)]): Seq[(String, Int, Int)] = {
        sentences match {
          case Nil => acc
          case x :: xs => loop(xs, x.length + count + 1, acc :+(x, count, x.length))
        }
      }

      loop(sentences, 0, Seq.empty[(String, Int, Int)])
    }

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

    //remove only this line, if you do not want to concat sentences.
    if (out.getAnnotations.size() > 1)
      out.setAnnotations(concatSuccessiveSameAnnotations(out.getAnnotations.toList))
    //remove only this line, if you do not want to concat sentences.

    out
  }

  private def concatSuccessiveSameAnnotations(annotations: List[BioCAnnotation]): List[BioCAnnotation] = {

    def arrangeAnnotationIds(annotations: List[BioCAnnotation]): List[BioCAnnotation] = {

      def loop(annots: List[BioCAnnotation], acc: List[BioCAnnotation], id: Int): List[BioCAnnotation] = annots match {
        case Nil => acc
        case x :: xs =>
          x.setID(id.toString)
          loop(xs, acc :+ x, id + 1)
      }

      val initialId = annotations.head.getID.toInt
      loop(annotations, List[BioCAnnotation](), initialId)
    }

    // a then b
    def successive(a: BioCAnnotation, b: BioCAnnotation): Boolean = {

      val endOfA = locationOf(a).getOffset + locationOf(a).getLength + 1
      val result = a.getInfon("PSIMI").equals(b.getInfon("PSIMI")) && endOfA == locationOf(b).getOffset

      result
    }

    def locationOf(a: BioCAnnotation): BioCLocation = a.getLocations.get(0)

    // a concat b
    def concatAnnotations(a: BioCAnnotation, b: BioCAnnotation): BioCAnnotation = {
      annotationId -= 1
      val out = new BioCAnnotation
      out.setID(a.getID)
      out.setInfons(a.getInfons)
      out.setText(a.getText + " " + b.getText)
      out.setLocation(locationOf(a).getOffset, locationOf(a).getLength + 1 + locationOf(b).getLength)
      out
    }

    def loop(annots: List[BioCAnnotation], acc: List[BioCAnnotation]): List[BioCAnnotation] = annots match {
      case Nil => acc
      case x :: xs if xs.isEmpty => loop(xs, acc :+ x)
      case x :: xs =>
        val y = xs.head
        if (successive(x, y)) {
          val newAnnot = concatAnnotations(x, y)
          loop(newAnnot :: xs.tail, acc)
        } else loop(xs, acc :+ x)
    }

    val result = loop(annotations, List[BioCAnnotation]())
    if (result.size > 1) arrangeAnnotationIds(result) else result

  }

}
