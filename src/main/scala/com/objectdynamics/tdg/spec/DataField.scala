package com.objectdynamics.tdg.spec

import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.spec.datatypes._

case class DataField(name: String,
                     dataType: DataType[_],
                     uniqueIn: Boolean,
                     dataIn: Option[List[String]]
                    ) extends IDataField {
  val fieldType: FieldType[_] = UnknownType()

  def this(name: String, dataType: DataType[_]) = {
    this(name, dataType, false, None)
  }

  override def unique: Boolean = uniqueIn

  override def data: Option[List[String]] = dataIn

  override def toString: String = "DataField(" + name + "," + dataType + ")"


  def withUnique(u: Boolean): DataField = {
    this.copy(uniqueIn = u)
  }

  //@deprecated
  //def makeLike(name: String): DataField = DataField(name, dataType, prefix, suffix, unique, data)
}