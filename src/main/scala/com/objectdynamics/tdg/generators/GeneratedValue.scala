package com.objectdynamics.tdg.generators

import java.util.Date

import com.objectdynamics.tdg.spec.datatypes._

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
sealed trait GeneratedValue[T <: DataType[_]] {
  type DType = T
  type ScalaType

  def dataType: T

  def value: Option[DType]

  def apply = value
}

trait TypedValue[T] extends GeneratedValue[T] {

}

trait NamedGeneratedValue[T] extends GeneratedValue[T] {
  def name: String
}

case class IntValue(num: Int, name: Option[String] = None) extends NamedGeneratedValue[IntType] {
  override type ScalaType = Int

  override def dataType: IntType = {
    IntType()
  }

  override def value: Option[ScalaType] = {
    Some(num)
  }
}

case class TextValue(text: String, name: Option[String] = None) extends NamedGeneratedValue[Text] {
  override type ScalaType = String

  override def dataType = {
    Text()
  }

  override def value: Option[ScalaType] = {
    Some(text)
  }
}

case class DateValue(name: String, value: Option[Date]) extends NamedGeneratedValue[Date] {
  override def dataType = Date
}

case object NullValue extends NamedGeneratedValue[NoType.type] {
  override val dataType = NoType
  override type ScalaType = this.type

  override def value: Option[NoType.type] = None

  def name: String = "<NULL>"
}

class valueFunction[I](gval: GeneratedValue[I]) extends (() => GeneratedValue[I]) {
  override def apply(): GeneratedValue[I] = gval
}
