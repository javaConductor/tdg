package com.objectdynamics.tdg.generators

import com.objectdynamics.tdg.builder.model.{DataType, ScalaInt, UnknownType}

/**
  * Created by IntelliJ IDEA.
  * User: lcollins
  * Date: 8/9/11
  * Time: 1:19 AM
  * To change this template use File | Settings | File Templates.
  */

/**
  * holds ONE generated field
  */
sealed trait GeneratedValue[U] {
  type ScalaType = U

  def name: String

  def dataType: DataType
  def apply:Option[U] = value
  def value: Option[U]
}
//
//  object GeneratedValues {
//    def nullValue() = new NullValue
//    def intValue(num: Int) = new IntValue(num)
//}

case class IntValue(num: Int, name: String) extends  GeneratedValue[Int] {
  override type ScalaType = Int

  override def dataType: ScalaInt = {
    ScalaInt()
  }

  override def value: Option[ScalaType] = {
    Some(num)
  }
}

case class NullValue() extends GeneratedValue[Nothing] {
  override val dataType = UnknownType
  override type ScalaType = Nothing
  override def value = None
  def name: String = "<NULL>"
}

class valueFunction[U](gval: GeneratedValue[U]) extends (() => Option[U]) {
  override def apply(): Option[U] = gval.value
}
