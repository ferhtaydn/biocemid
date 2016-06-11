package com.ferhtaydn.biocemid

import java.io.{ FileOutputStream, FileReader, OutputStreamWriter }
import java.nio.charset.StandardCharsets

import bioc.{ BioCCollection, BioCDocument }
import bioc.io.{ BioCDocumentReader, BioCDocumentWriter, BioCFactory }
import com.ferhtaydn.biocemid.annotators.baseline.{ BaselineAnnotator, BaselineAnnotatorConfig }
import com.ferhtaydn.biocemid.annotators.tfrf.{ TfrfAnnotator, TfrfAnnotatorConfig }
import com.ferhtaydn.biocemid.annotators.word2vec.{ Word2vecAnnotator, Word2vecAnnotatorConfig }

import scala.collection.JavaConversions._

package object annotators {

  def annotate(annotatorConfig: AnnotatorConfig): Unit = {

    val annotator = annotatorConfig match {
      case baselineConfigs: BaselineAnnotatorConfig ⇒ new BaselineAnnotator(baselineConfigs)
      case tfrfConfigs: TfrfAnnotatorConfig         ⇒ new TfrfAnnotator(tfrfConfigs)
      case word2vecConfigs: Word2vecAnnotatorConfig ⇒ new Word2vecAnnotator(word2vecConfigs)
    }

    annotate(annotator)
  }

  private def annotate(annotator: Annotator): Unit = {

    list(annotator.config.rawDirectory, xmlSuffix).foreach { file ⇒

      val fileName = extractFileName(file.getName, xmlSuffix)
      val out = s"${annotator.config.rawDirectory}/${fileName}_${annotator.config.outputFileSuffix}"

      val factory: BioCFactory = BioCFactory.newFactory(BioCFactory.WOODSTOX)
      val reader: BioCDocumentReader = factory.createBioCDocumentReader(new FileReader(file))
      val writer: BioCDocumentWriter = factory.createBioCDocumentWriter(
        new OutputStreamWriter(new FileOutputStream(out), StandardCharsets.UTF_8)
      )

      val collection: BioCCollection = reader.readCollectionInfo

      annotator.resetAnnotationId()
      val outCollection: BioCCollection = annotator.getCollection(collection)
      outCollection.setKey("sentence.key")
      writer.writeCollectionInfo(outCollection)

      for (document ← reader) {
        val outDocument: BioCDocument = annotator.getDocument(document)
        writer.writeDocument(outDocument)
      }

      reader.close()
      writer.close()

    }
  }

}
