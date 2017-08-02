package com.objectdynamics.tdg.builder.model

/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/9/11
 * Time: 1:04 AM
 */
;

trait DataSet {
  //def this(dss:B, withRows:List[Z]){}
  def row(idx: Int): Option[DataRow];

  def rows: List[DataRow];

  def +(r: DataRow): DataSet;

  def +(r: List[DataRow]): DataSet;

  def name: String;

  def dataObjectSpec: IDataSetSpec;

}