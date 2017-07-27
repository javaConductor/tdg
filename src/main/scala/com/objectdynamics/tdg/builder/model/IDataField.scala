package com.objectdynamics.tdg.builder.model

/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/9/11
 * Time: 12:45 AM
 */

import com.objectdynamics.tdg.spec.datatypes._

trait IDataField /*[+D <: IDataTypeObject ]*/ {
  val name: String
  val fieldType: FieldType
  val dataType: IDataTypeInstance

  def data: Option[List[_]] = None

  def unique: Boolean = false

  def prefix: Option[String] = None

  def suffix: Option[String] = None

  @deprecated
  def makeLike(name: String): IDataField

}
