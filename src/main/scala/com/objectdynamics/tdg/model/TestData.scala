/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.objectdynamics.tdg.model

import com.objectdynamics.tdg.builder.model._

/**
 * This class represents one generations worth of generated data.
 *
 *
 * trait ITestData[
B <: IDataSetSpec,
C <: IDataField,
D <: IDataTypeObject,
E <: IDataTypeInstance,
Y <: IDataSet[B, C, D, E, Z],
Z <: IDataRow[B, C, D, E]]
 */
case class TestData(dataSets: Set[IDataSet])
  extends ITestData
{
    val dsMap: Map[String, IDataSet] = dataSets map
      {
          ds: IDataSet => (ds.name -> ds)
      } toMap;

    def +(ds: IDataSet): ITestData = TestData(dataSets + ds);

    def dataSet(dsName: String): Option[IDataSet] = dsMap.get(dsName);

    def dataSetList = this.dataSets.toList;

}
