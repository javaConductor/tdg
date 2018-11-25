package com.objectdynamics.tdg.converters

import com.objectdynamics.tdg.builder.model.{DataRow, DefaultDataSet, IDataSetSpec}
import com.objectdynamics.tdg.model.DefaultTestData

/**
  * Created by lee on 11/13/18.
  */
sealed trait TestDataConverter[T] {
  def apply(testData: DefaultTestData): T
}

class CSVDataConverter extends TestDataConverter[Map[String, List[String]]] {

  def createDataRow(fldNames: List[String], row: DataRow): String = {
    fldNames.map(fldName => row.value(fldName) match {
      case Some(gval) => gval.value
      case _ => throw new Exception("Field name [" + fldName + "] not found in data.")
    }).mkString(",")
  }

  def createDataRows(ds: DefaultDataSet): List[String] = {
    ds.dataRows.map(row => createDataRow(fieldNames(ds.dataObjSpec), row))
  }

  def createRows(ds: DefaultDataSet): List[String] = {
    ds.dataObjectSpec.fields.map(_.name).mkString(",") :: createDataRows(ds)
  }

  def apply(testData: DefaultTestData): Map[String, List[String]] = {
    testData.dataSets.foldLeft(Map[String, List[String]]())((m, ds) => {
      m + (ds.name -> createRows(ds))
    })

  }

  def fieldNames(dataSetSpec: IDataSetSpec): List[String] = dataSetSpec.fields.map(df => df.name)
}

//
//class StreamDataConverter extends TestDataConverter[Stream[Map[String, _]]] {
//  def apply[T](testData: DefaultTestData): T = {}
//}
//
//class JSONDataConverter extends TestDataConverter[String] {
//  def apply(testData: DefaultTestData): String = {}
//}
