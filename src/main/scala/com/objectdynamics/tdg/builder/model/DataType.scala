/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/13/11
 * Time: 1:54 AM
 */
package com.objectdynamics.tdg.builder.model

import com.objectdynamics.tdg.builder.BuilderException
import org.joda.time.DateTime

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
  implicit object DefaultString extends Default[String]("")
  implicit object DefaultByte extends Default[Byte](0)
  implicit object DefaultChar extends Default[Char]('\u0000')
  implicit object DefaultBoolean extends Default[Boolean](false)
  implicit object DefaultUnit extends Default[Unit](())

  implicit def defaultSeq[A]: Default[Seq[A]] = new Default[Seq[A]](Seq())
  implicit def defaultSet[A]: Default[Set[A]] = new Default[Set[A]](Set())
  implicit def defaultMap[A, B]: Default[Map[A, B]] = new Default[Map[A, B]](Map[A, B]())
  implicit def defaultOption[A]: Default[Option[A]] = new Default[Option[A]](None)

  def value[A](implicit value: Default[A]): A = value.default
}

sealed trait TypeDef {
  val name:String
  type ScalaType
}

sealed trait DataType extends TypeDef {
  type ScalaType
  type DateType = DateTime
  type Aux[T] = DataType{type ScalaType=T}
  def defaultValue(implicit value: Default[ScalaType]) : ScalaType = {
    value.default
  }
}

class DefaultDataType[U](val name: String) extends DataType {
  override type ScalaType = U
}

case class UnknownType() extends DefaultDataType[Nothing]("--UNKNOWN--"){override type ScalaType = Nothing}
case class ScalaInt() extends DefaultDataType[Int]("Int"){override type ScalaType = Int}
case class  ScalaString() extends DefaultDataType[String]("String"){override type ScalaType = String}
case class  ScalaFloat() extends DefaultDataType[Float]("Float"){override type ScalaType = Float}
case class  ScalaDouble() extends DefaultDataType[Double]("Double"){override type ScalaType = Double}
case class  ScalaBigDecimal() extends DefaultDataType[BigDecimal]("Decimal"){override type ScalaType = BigDecimal}
case class  ScalaBoolean() extends DefaultDataType[Boolean]("Boolean"){override type ScalaType = Boolean}
case class  ScalaDateTime() extends DefaultDataType[DateTime]("DateTime"){override type ScalaType = DateTime}
case class ScalaSeq[T](content:DefaultDataType[T]) extends DefaultDataType[Seq[T]](s"Seq[$content.name]"){override type ScalaType = Seq[T]}
case class ScalaMap(keyType:DefaultDataType[_], valueType:DefaultDataType[_]) extends DefaultDataType[Map[_,_]](s"Scala Map[${keyType.name}, ${valueType.name}]"){override type ScalaType = Map[_,_]}
case class ScalaType[T](c: Class[T]) extends DefaultDataType[T](s"Class[${c.getName}]"){override type ScalaType = T}
object UnknownType extends UnknownType

object DataType {

  def fromString(dtype: String, size: Int): DataType = {
    dtype match {
      case ("Int") => ScalaInt()
      case ("Integer") => ScalaInt()
      case ("String") => ScalaString()
      case ("Date") => ScalaDateTime()
      case "Decimal" if size > 0 => ScalaBigDecimal()
      case ("Float") => ScalaFloat()
      case ("Double") => ScalaDouble()
      case ("BigDecimal") => ScalaBigDecimal()
      case s: String => throw new BuilderException("No such DataType:" + s)
    }
  }
}
