package com.objectdynamics.tdg.schema

import com.objectdynamics.tdg.builder.model._

/**
 *
 * This class represents the effective schema definition for
 * the running of the app.
 */
case class TestDataSchema(val name: String, val dssMap: Map[String, IDataSetSpec] = Map[String, IDataSetSpec]())
  extends ITestDataSchema
{
    //def apply(name: String) =TestDataSchema(name, Map[String, DataSetSpec]());
    def dataSetSpecs: Map[String, IDataSetSpec] = dssMap;

    def +(dss: IDataSetSpec): ITestDataSchema =
    {
        new TestDataSchema(name, dssMap + (dss.name -> dss))
    }

    def dataSetSpec(name: String): Option[IDataSetSpec] = dssMap.get(name)

}