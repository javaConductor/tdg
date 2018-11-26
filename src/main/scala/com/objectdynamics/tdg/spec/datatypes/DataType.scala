/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/13/11
 * Time: 1:54 AM
 */
package com.objectdynamics.tdg.spec.datatypes

import org.joda.time.DateTime

import scala.reflect.runtime.universe._

class Default[+A](val default: A)

trait LowerPriorityImplicits {
  // Stop AnyRefs from clashing with AnyVals
  implicit def defaultNull[A <: AnyRef]: Default[A] = new Default[A](null.asInstanceOf[A])
}

object Default extends LowerPriorityImplicits {

  implicit object DefaultDouble extends Default[Double](0.0)

  implicit object DefaultFloat extends Default[Float](0.0F)

  implicit object DefaultInt extends Default[Int](0)

  implicit object DefaultLong extends Default[Long](0L)

  implicit object DefaultShort extends Default[Short](0)

  implicit object DefaultString extends Default[String]("")

  implicit object DefaultByte extends Default[Byte](0)

  implicit object DefaultChar extends Default[Char]('\u0000')

  implicit object DefaultBoolean extends Default[Boolean](false)

  implicit object DefaultUnit extends Default[Unit](())

  implicit def defaultSeq[A]: Default[Seq[A]] = new Default[Seq[A]](Seq())

  implicit def defaultSet[A]: Default[Set[A]] = new Default[Set[A]](Set())

  implicit def defaultMap[A, B]: Default[Map[A, B]] = new Default[Map[A, B]](Map[A, B]())

  implicit def defaultOption[A]: Default[Option[A]] = new Default[Option[A]](None)

  def value[A](implicit value: Default[A], tag: WeakTypeTag[A]): A = value.default
}

sealed trait TypeDef {
  type ScalaType
  val name: String
}

sealed trait DataType extends TypeDef {
  type ScalaType
  type DateType = DateTime
  type Aux[T] = DataType {type ScalaType = T}

  def defaultValue(implicit tag: WeakTypeTag[ScalaType], value: Default[ScalaType]): ScalaType = {
    value.default
  }
}

class DefaultDataType[U](val name: String) extends DataType {
  override type ScalaType = U
}

case class UnknownType() extends DefaultDataType[Nothing]("--UNKNOWN--") {
  override type ScalaType = Nothing
}

case class ScalaInt() extends DefaultDataType[Int](classOf[Int].getName) {
  override type ScalaType = Int
}

case class ScalaShort() extends DefaultDataType[Short](classOf[Short].getName) {
  override type ScalaType = Short
}

case class ScalaString() extends DefaultDataType[String](classOf[String].getName) {
  override type ScalaType = String
}

case class ScalaFloat() extends DefaultDataType[Float](classOf[Float].getName) {
  override type ScalaType = Float
}

case class ScalaDouble() extends DefaultDataType[Double](classOf[Double].getName) {
  override type ScalaType = Double
}

case class ScalaBigDecimal() extends DefaultDataType[BigDecimal](classOf[BigDecimal].getName) {
  override type ScalaType = BigDecimal
}

case class ScalaBoolean() extends DefaultDataType[Boolean](classOf[Boolean].getName) {
  override type ScalaType = Boolean
}

case class ScalaDateTime() extends DefaultDataType[DateTime](classOf[DateTime].getName) {
  override type ScalaType = DateTime
}

case class ScalaSeq[T](content: DefaultDataType[T]) extends DefaultDataType[Seq[T]](s"Seq[$content.name]") {
  override type ScalaType = Seq[T]
}

case class ScalaMap(keyType: DefaultDataType[_], valueType: DefaultDataType[_]) extends DefaultDataType[Map[_, _]](s"Scala Map[${keyType.name}, ${valueType.name}]") {
  override type ScalaType = Map[_, _]
}

case class ScalaType[T](c: Class[T]) extends DefaultDataType[T](s"Class[${c.getName}]") {
  override type ScalaType = T
}

object UnknownType extends UnknownType

object DataType {

  val simpleTypes = Map[String, Any](
    classOf[Int].getName -> ScalaInt,
    classOf[Double].getName -> ScalaDouble,
    classOf[String].getName -> ScalaString,
    classOf[Float].getName -> ScalaDouble,
    classOf[BigDecimal].getName -> ScalaBigDecimal,
    classOf[DateTime].getName -> ScalaDateTime,
    classOf[Short].getName -> ScalaShort)

  def isSimpleClass[T](c: Class[T]) = simpleTypes(c.getName)

  def fromClass[T](c: Class[T]): Option[DataType] = {
    if (simpleTypes.keySet.contains(c.getName))
      fromString(c.getName)
    else
      Some(ScalaType[T](c))
  }

  def fromString(dtype: String, size: Int = 0): Option[DataType] = {
    //TODO fix this
    simpleTypes(dtype).asInstanceOf[Option[DataType]]
  }
}
