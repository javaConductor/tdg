/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/13/11
 * Time: 1:54 AM
 */
package com.objectdynamics.tdg.builder.model

sealed trait FieldType[T] {
type ScalaType = T
  val defaultvalue = implicitly[Default[T]]()
  //TODO add to/from String methods
}

case class UnknownType() extends FieldType[Nothing]
case class ScalaInt() extends FieldType[Int]
case class  ScalaString() extends FieldType[String]
case class  ScalaFloat() extends FieldType[Float]
case class  ScalaDouble() extends FieldType[Double]
case class  ScalaBoolean() extends FieldType[Boolean]
case class ScalaSeq[T](content:FieldType[T]) extends FieldType[Seq[T]]
case class ScalaMap(keyType:FieldType[_],valueType:FieldType[_]) extends FieldType[Map[_,_]]

case class ScalaType[T](c: Class[T]) extends FieldType[T]

object FieldTypes{
}


class Default[+A](val default: A)

trait LowerPriorityImplicits {
  // Stop AnyRefs from clashing with AnyVals
  implicit def defaultNull[A <: AnyRef]:Default[A] = new Default[A](null.asInstanceOf[A])  
}

object Default extends LowerPriorityImplicits {
  implicit object DefaultDouble extends Default[Double](0.0)
  implicit object DefaultFloat extends Default[Float](0.0F)
  implicit object DefaultInt extends Default[Int](0)
  implicit object DefaultLong extends Default[Long](0L)
  implicit object DefaultShort extends Default[Short](0)
  implicit object DefaultByte extends Default[Byte](0)
  implicit object DefaultChar extends Default[Char]('\u0000')
  implicit object DefaultBoolean extends Default[Boolean](false)
  implicit object DefaultUnit extends Default[Unit](())

  implicit def defaultSeq[A]: Default[immutable.Seq[A]] = new Default[immutable.Seq[A]](immutable.Seq())
  implicit def defaultSet[A]: Default[Set[A]] = new Default[Set[A]](Set())
  implicit def defaultMap[A, B]: Default[Map[A, B]] = new Default[Map[A, B]](Map[A, B]())
  implicit def defaultOption[A]: Default[Option[A]] = new Default[Option[A]](None)

  def value[A](implicit value: Default[A]): A = value.default
}
