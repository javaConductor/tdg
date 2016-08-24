/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.objectdynamics.tdg.model

import com.objectdynamics.tdg.builder.model._

import scala.util.control.TailCalls.TailRec

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
  extends ITestData
{
    val dsMap: Map[String, IDataSet] = dataSets map {
          ds: IDataSet => (ds.name -> ds)
      } toMap;

    def +(ds: IDataSet): ITestData = TestData(dataSets + ds);

    def dataSet(dsName: String): Option[IDataSet] = dsMap.get(dsName);

    def dataSetList = this.dataSets.toList;

    def eachRow(dataSetName:String, fn: Function2[Int, IDataRow, Any]) = {
      dataSet(dataSetName) match {
        case Some(dataSet:IDataSet) => (
          eachRow(dataSet.rows,fn)
        )
        case _ => { sys.error("No such dataSet: " + dataSetName)}
        }
    };

  @TailRec
  def eachRow(dataRows: List[IDataRow], fn: Function2[Int, IDataRow, Any], n:Int = 0):Int = {
    if (!dataRows.isEmpty) {
      try {
        fn(n, dataRows.head)
      } catch {
        case t: Throwable => ( sys.error("TestData.eachRow(): element "+n+ " threw exception: "+ t.getMessage))
      };
      return eachRow( dataRows.tail, fn, n+1)
    }
    n
  }

  /// how to seamlessly stream test data to/from MongoDB
  /// do it in DataSet
}
