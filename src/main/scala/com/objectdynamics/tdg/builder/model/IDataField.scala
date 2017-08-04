package com.objectdynamics.tdg.builder.model

/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/9/11
 * Time: 12:45 AM
 */
trait IDataField /*[+D <: IDataTypeObject ]*/ {
  val name: String
  val dataType: DataType

  def data: Option[List[_]] = None
  def unique: Boolean = false
  def prefix: Option[String] = None
  def suffix: Option[String] = None
}
