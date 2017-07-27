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
  * B <: IDataSetSpec,
  * C <: IDataField,
  * D <: IDataTypeObject,
  * E <: IDataTypeInstance,
  * Y <: IDataSet[B, C, D, E, Z],
  * Z <: IDataRow[B, C, D, E]]
  */
case class TestData(dataSets: Set[IDataSet])
  extends ITestData {

  val dsMap: Map[String, IDataSet] = dataSets map { ds => ds.name -> ds } toMap

  def +(ds: IDataSet): ITestData = withDataSet(ds)

  def dataSet(dsName: String): Option[IDataSet] = dsMap.get(dsName);

  def dataSetList = this.dataSets.toList

  def withDataSet(dataSet: IDataSet) = TestData(dataSets.map { ds => if (dataSet.name == ds.name) dataSet else ds })
}
