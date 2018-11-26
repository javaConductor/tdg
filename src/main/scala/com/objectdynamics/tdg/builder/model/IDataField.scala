package com.objectdynamics.tdg.builder.model

import com.objectdynamics.tdg.spec.datatypes.DataType

/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/9/11
 * Time: 12:45 AM
 */
trait IDataField /*[+D <: IDataTypeObject ]*/ {
  val name: String
  val dataType: DataType

  def unique: Boolean = false

  def prefix: Option[String] = None

  def suffix: Option[String] = None
}
