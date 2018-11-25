package com.objectdynamics.tdg.targets

import java.io.{File, FileWriter}
import java.time.LocalDateTime

import com.objectdynamics.tdg.builder.model.DefaultDataSet
import com.objectdynamics.tdg.converters.CSVDataConverter
import com.objectdynamics.tdg.model.DefaultTestData

/**
  * Created by lee on 11/24/18.
  */

case class DataSetManifest(outputFileName: String, fileSize: Long, numRows:Long, format: String, when: LocalDateTime);

case class WriteManifest(dataSetList: Seq[DataSetManifest])

trait DataTarget[C] {
  val converter: C

  def store(testData: DefaultTestData): WriteManifest
}

class CSVFileDataTarget(folderName: String) extends DataTarget[CSVDataConverter] {
  val converter = new CSVDataConverter()

  def store(testData: DefaultTestData): WriteManifest = {

    val folder = new File(folderName)
    if (!folder.exists()) {
      try {
        folder.mkdir()
      } catch {
        case e: Exception => throw new IllegalArgumentException("Cannot create output folder: [" + folderName + "]: " + e.getMessage)
      }

    } else {
      if (!folder.canWrite) throw new IllegalArgumentException("Cannot write to output folder: [" + folderName + "]")
    }

    WriteManifest(testData.dataSetList.map(store(folder, _)))
  }

  def store(folder: File, dataSet: DefaultDataSet): DataSetManifest = {

    val f = new File(folder, dataSet.name+".csv")
    if(!f.exists())
      f.createNewFile()
    val nRows = dataSet.dataRows.size

    // converter
    val textRows = converter.createRows(dataSet)

    /// open file
    val fw = new FileWriter(f)
    textRows.foreach( r => {fw.write(r); fw.append('\n') })
    fw.close()
    DataSetManifest(f.getAbsolutePath, f.length(), nRows, "text/csv", LocalDateTime.now())  }


}

