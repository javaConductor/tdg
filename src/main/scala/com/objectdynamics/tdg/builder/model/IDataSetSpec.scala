package com.objectdynamics.tdg.builder.model

import com.objectdynamics.tdg.spec._

trait IDataSetSpec {

  val name: String
  var dataSetType: DataSetType

  def fields: List[IDataField]

  def field(fldName: String): Option[IDataField]

  def fieldNames: List[String]

  def definesFields(flds: Set[String]): Boolean = flds.size == (fields intersect flds.toList).length

}
