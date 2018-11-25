/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.objectdynamics.tdg.model

import com.objectdynamics.tdg.builder.model.{DataSet, DefaultDataSet}

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

  def +(ds: DefaultDataSet): TestData[DefaultDataSet] = withDataSet(ds)

  def dataSet(dsName: String): Option[DefaultDataSet] = dsMap.get(dsName);

  def dataSetList = this.dataSets

  def withDataSet(dataSet: DefaultDataSet) =
    //TODO the last dataset with a given name wins
    DefaultTestData((dataSet :: dataSets.toList).toSet.toList)
}
