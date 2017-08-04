/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.objectdynamics.tdg.model

import com.objectdynamics.tdg.builder.model.{DataSet, DefaultDataSet}

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

trait TestData[DSType <: DataSet[_,_]] {
  type DS = DSType

  def +(ds: DS): TestData[DS]

  def dataSet(dsName: String): Option[DS]

  def dataSetList: Seq[DS]

}

/**
  * This class represents one generations worth of generated data.
  *
  */
case class DefaultTestData(dataSets: Seq[DefaultDataSet])
  extends TestData[DefaultDataSet] {

  def this() = this(Seq[DefaultDataSet]())
  val dsMap: Map[String, DefaultDataSet] = dataSets map { ds => ds.name -> ds } toMap

  def +(ds: DS): TestData[DS] = withDataSet(ds)

  def dataSet(dsName: String): Option[DefaultDataSet] = dsMap.get(dsName);

  def dataSetList = this.dataSets

  def withDataSet(dataSet: DS) =
    DefaultTestData(dataSets.map { ds => if (dataSet.name == ds.name) dataSet else ds })
}
