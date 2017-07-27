package com.objectdynamics.tdg.builder.model

/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/9/11
 * Time: 12:50 AM
 */
;

trait ITestDataSchema {
  def +(dss: IDataSetSpec): ITestDataSchema;

  def dataSetSpec(name: String): Option[IDataSetSpec];

  def dataSetSpecs: Map[String, IDataSetSpec];
}