package com.objectdynamics.tdg.spec

import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.spec.datatypes.DataType

case class DataField(name: String,
                     dataType: DataType,
                     uniqueIn: Boolean,
                     dataIn: Option[List[String]]
                    ) extends IDataField {

  def this(name: String, dataType: DataType) = {
    this(name, dataType, false, None)
  }

  override def unique: Boolean = uniqueIn

  override def toString: String = "DataField(" + name + "," + dataType + ")"

  def withUnique(u: Boolean): DataField = {
    this.copy(uniqueIn = u)
  }

}