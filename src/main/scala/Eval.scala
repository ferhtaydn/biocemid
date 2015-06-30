import java.io.{FileOutputStream, OutputStreamWriter, FileReader}

import bioc.{BioCDocument, BioCCollection}
import bioc.io.{BioCDocumentWriter, BioCDocumentReader, BioCFactory}

object Eval extends App {

  val directory = args(0)

  annotate(directory)

  def annotate(dir: String) = {

    IO.list(dir, ".xml").foreach { file =>

      val fileName = Util.extractFileName(file.getName, ".xml")
      val out = s"$dir/${fileName}_out.xml"

      val factory: BioCFactory = BioCFactory.newFactory(BioCFactory.WOODSTOX)
      val reader: BioCDocumentReader = factory.createBioCDocumentReader(new FileReader(file))
      val writer: BioCDocumentWriter = factory.createBioCDocumentWriter(new OutputStreamWriter(new FileOutputStream(out), "UTF-8"))

      val collection: BioCCollection = reader.readCollectionInfo

      val converter: SentenceConverter = new SentenceConverter

      val outCollection: BioCCollection = converter.getCollection(collection)
      outCollection.setKey("sentence.key")
      writer.writeCollectionInfo(outCollection)

      import scala.collection.JavaConversions._
      for (document <- reader) {
        val outDocument: BioCDocument = converter.getDocument(document)
        writer.writeDocument(outDocument)
      }

      reader.close()
      writer.close()

    }
  }

}
