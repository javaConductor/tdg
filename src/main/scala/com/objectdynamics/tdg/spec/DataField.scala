package com.objectdynamics.tdg.spec

import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.spec.datatypes._

case class DataField(name: String,
                     dataType: DataType[_],
                     prefixIn: Option[String],
                     suffixIn: Option[String],
                     uniqueIn: Boolean,
                     dataIn: Option[List[String]]
                    ) extends IDataField {
  val fieldType: FieldType = ASingleValue

  def this(name: String, dataType: IDataTypeInstance) = {
    this(name, dataType, None, None, None, None, None, false, None, None)
  }

  override def prefix: Option[String] = prefixIn

  override def suffix: Option[String] = suffixIn

  override def unique: Boolean = uniqueIn

  override def data: Option[List[String]] = dataIn

  override def toString: String = "DataField(" + name + "," + dataType + ")"


  def withUnique(u: Boolean): DataField = {
    this.copy(uniqueIn = u)
  }

  def withSuffix(m: Option[String]): DataField = {
    this.copy(suffixIn = m)
  }

  def withPrefix(px: Option[String]): DataField = {
    this.copy(prefixIn = px)
  }

  @deprecated
  def makeLike(name: String): DataField = DataField(name, dataType, prefix, suffix, unique, data)
}