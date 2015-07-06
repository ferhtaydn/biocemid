
import java.io.{FileOutputStream, OutputStreamWriter, FileReader, File}
import bioc.{BioCDocument, BioCCollection}
import bioc.io.{BioCDocumentWriter, BioCDocumentReader, BioCFactory}
import com.typesafe.config.ConfigFactory

import scala.collection.JavaConversions._
import scala.xml.XML

object BioC {

  val config = ConfigFactory.load()

  val MI_0018_Synonym: List[String] = config.getStringList("BioC.PSIMI.0018.Synonym").toList
  val MI_0019_Synonym: List[String] = config.getStringList("BioC.PSIMI.0019.Synonym").toList
  val MI_0096_Synonym: List[String] = config.getStringList("BioC.PSIMI.0096.Synonym").toList
  val MI_0416_Synonym: List[String] = config.getStringList("BioC.PSIMI.0416.Synonym").toList
  val MI_0040_Synonym: List[String] = config.getStringList("BioC.PSIMI.0040.Synonym").toList
  val MI_0055_Synonym: List[String] = config.getStringList("BioC.PSIMI.0055.Synonym").toList
  val MI_0402_Synonym: List[String] = config.getStringList("BioC.PSIMI.0402.Synonym").toList
  val MI_0114_Synonym: List[String] = config.getStringList("BioC.PSIMI.0114.Synonym").toList

  val MI_0018_Related: List[String] = config.getStringList("BioC.PSIMI.0018.Related").toList
  val MI_0019_Related: List[String] = config.getStringList("BioC.PSIMI.0019.Related").toList
  val MI_0096_Related: List[String] = config.getStringList("BioC.PSIMI.0096.Related").toList
  val MI_0416_Related: List[String] = config.getStringList("BioC.PSIMI.0416.Related").toList
  val MI_0040_Related: List[String] = config.getStringList("BioC.PSIMI.0040.Related").toList
  val MI_0055_Related: List[String] = config.getStringList("BioC.PSIMI.0055.Related").toList
  val MI_0402_Related: List[String] = config.getStringList("BioC.PSIMI.0402.Related").toList
  val MI_0114_Related: List[String] = config.getStringList("BioC.PSIMI.0114.Related").toList

  val MI_0018_Extra: List[String] = config.getStringList("BioC.PSIMI.0018.Extra").toList
  val MI_0019_Extra: List[String] = config.getStringList("BioC.PSIMI.0019.Extra").toList
  val MI_0096_Extra: List[String] = config.getStringList("BioC.PSIMI.0096.Extra").toList
  val MI_0416_Extra: List[String] = config.getStringList("BioC.PSIMI.0416.Extra").toList
  val MI_0040_Extra: List[String] = config.getStringList("BioC.PSIMI.0040.Extra").toList
  val MI_0055_Extra: List[String] = config.getStringList("BioC.PSIMI.0055.Extra").toList
  val MI_0402_Extra: List[String] = config.getStringList("BioC.PSIMI.0402.Extra").toList
  val MI_0114_Extra: List[String] = config.getStringList("BioC.PSIMI.0114.Extra").toList

  val vocabularies: Map[String, (List[String], List[String], List[String])] = Map(
    ("0018", (MI_0018_Synonym, MI_0018_Related, MI_0018_Extra)),
    ("0019", (MI_0019_Synonym, MI_0019_Related, MI_0019_Extra)),
    ("0096", (MI_0096_Synonym, MI_0096_Related, MI_0096_Extra)),
    ("0416", (MI_0416_Synonym, MI_0416_Related, MI_0416_Extra)),
    ("0040", (MI_0040_Synonym, MI_0040_Related, MI_0040_Extra)),
    ("0055", (MI_0055_Synonym, MI_0055_Related, MI_0055_Extra)),
    ("0402", (MI_0402_Synonym, MI_0402_Related, MI_0402_Extra)),
    ("0114", (MI_0114_Synonym, MI_0114_Related, MI_0114_Extra))
  )

  def getFrequencies(wordsFile: String): Seq[(String, Double)] = {
    val groupedWords = IO.read(wordsFile).foldLeft(Map.empty[String, Double]) {
      (m, word) => m + (word -> (m.getOrElse(word, 0.0) + 1.0))
    }
    groupedWords.toSeq.sortBy(_._2).reverse
  }

  def extractAnnotatedSentences(file: File, method: String): String = {

    val bioCFile = XML.loadFile(file)
    val passages = bioCFile \\ "passage"
    val annotations = passages \\ "annotation"

    val filtered = annotations.filter(annot =>
      (annot \ "infon").filter(i =>
        (i \\ "@key").text.equals("PSIMI")
      ).text.equals(method)
    )

    filtered.map(m => (m \\ "text").text).mkString("\n")

  }

  def tfRf(method: String, wordsFile: String): Seq[(String, Double)] = {

    val methodFreqs = BioC.getFrequencies(wordsFile)
    val otherFiles = IO.listOthers(method).map(f => f.getPath)

    methodFreqs.map { case (word, freq) =>

      def log2(x: Double) = scala.math.log(x) / scala.math.log(2)

      val otherTotal = otherFiles.map(BioC.getFrequencies(_).find(wf => word.equals(wf._1)).fold(0.0)(_._2)).sum

      val rf = log2(2.0 + (freq / scala.math.max(1.0, otherTotal)))
      val tfRf = freq * rf

      (word, tfRf)
    }
  }

  def annotate(dir: String) = {

    IO.list(dir, ".xml").foreach { file =>

      val fileName = Util.extractFileName(file.getName, ".xml")
      val out = s"$dir/${fileName}_out.xml"

      val factory: BioCFactory = BioCFactory.newFactory(BioCFactory.WOODSTOX)
      val reader: BioCDocumentReader = factory.createBioCDocumentReader(new FileReader(file))
      val writer: BioCDocumentWriter = factory.createBioCDocumentWriter(
        new OutputStreamWriter(new FileOutputStream(out), "UTF-8")
      )

      val collection: BioCCollection = reader.readCollectionInfo

      val converter: SentenceConverter = new SentenceConverter

      val outCollection: BioCCollection = converter.getCollection(collection)
      outCollection.setKey("sentence.key")
      writer.writeCollectionInfo(outCollection)

      for (document <- reader) {
        val outDocument: BioCDocument = converter.getDocument(document)
        writer.writeDocument(outDocument)
      }

      reader.close()
      writer.close()

    }
  }
}
