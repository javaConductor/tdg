/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/13/11
 * Time: 1:54 AM
 */
package com.objectdynamics.tdg.builder.model

sealed trait FieldType[T] {
type ScalaType = T
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
