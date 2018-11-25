package com.objectdynamics.tdg.schema

import com.objectdynamics.tdg.builder.model._

/**
  *
  * This class represents the effective schema definition for
  * the running of the app.
  */
case class TestDataSchema(schemaName: String, dssMap: Map[String, IDataSetSpec] = Map[String, IDataSetSpec]())
  extends ITestDataSchema {
  def dataSetSpecs: Map[String, IDataSetSpec] = dssMap

  def +(dss: IDataSetSpec): ITestDataSchema = {
    TestDataSchema(schemaName, dssMap + (dss.name -> dss))
  }

  def dataSetSpec(name: String): Option[IDataSetSpec] = dssMap.get(name)

}