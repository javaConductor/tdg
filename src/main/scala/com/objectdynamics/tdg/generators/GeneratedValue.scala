package com.objectdynamics.tdg.generators

import com.objectdynamics.tdg.spec.datatypes._

/**
  * holds ONE generated field
  */
sealed trait GeneratedValue {

  def name: String
  def dataType: DataType

  def apply = value
  //def apply[T] = as[T]

  def as[T](implicit valueExtractor: ValueExtractor[T]): T  = {valueExtractor.value(this)}
  def value: String
}

case class IntValue(num: Int, name: String) extends GeneratedValue {
  override def dataType =  ScalaInt()
  override def value: String = num.toString
}


case class FloatValue(num: Double, name: String) extends GeneratedValue {
  override def dataType =  ScalaFloat()
  override def value: String = num.toString
}

case class DoubleValue(num: Double, name: String) extends GeneratedValue {
  override def dataType =  ScalaDouble()
  override def value: String = num.toString
}


case class StringValue(str: String, name: String) extends GeneratedValue {

  override def dataType = ScalaString()

  override def value: String = str
}

case class NullValue() extends GeneratedValue {
  override val dataType = UnknownType

  override def value:Nothing = ???

  def name: String = "<NULL>"
}



