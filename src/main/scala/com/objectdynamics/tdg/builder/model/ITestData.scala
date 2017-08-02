package com.objectdynamics.tdg.builder.model

/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/9/11
 * Time: 1:09 AM
 */

trait ITestData {
  def +(ds: DataSet): ITestData;

  def dataSet(dsName: String): Option[DataSet];

  def dataSetList: List[DataSet];

}