package com.objectdynamics.tdg.storage

import com.objectdynamics.tdg.builder.model.IDataRow
/**
  * The Storage object for a DataSet of a request
  */
trait Storage {

  def getDataSetName():String;
  def getRequestId():String;
  def getRow(i:Int):IDataRow;
  def findGroups(fields:List[String]):List[Group];
  def getGroup(group: Group): List[IDataRow];
  def updateRow(i:Int, iDataRow: IDataRow);
}

case class Group(keys: Map[String, Any], count: Long ) {

}
