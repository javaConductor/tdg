package com.objectdynamics.tdg.spec.datatypes

import java.text.SimpleDateFormat

import com.objectdynamics.tdg.builder._
import com.objectdynamics.tdg.generators._

/**
  * Created by IntelliJ IDEA.
  * User: Lee
  * Date: Nov 19, 2010
  * Time: 1:22:10 AM
  *
  */
trait IDataTypeObject {
  val name: String
  def toString(v: GeneratedValue[_]): String = ""

  def objectFromString(strRep: String): Option[ScalaClass] = None

  def fromString(s: String, size: Int): Option[_] = None

  def fromString(dtype: String): Option[_] = fromString(dtype, 0)

  type DateClass = java.util.Date
  type ScalaClass
}

object DataType {

  def fromString(dtype: String, size: Int): DataType[_] = {
    dtype match {
      case ("Int") => IntType()
      case ("Integer") => IntType()
      case ("Date") => DateType()
      case "Decimal" if size > 0 => Decimal(size)
      case ("Float") => Float
      case ("Text") => Text(0)
      case ("String") => Text(0)
      case s: String => throw new BuilderException("No such DataType:" + s)
    }
  }

}


sealed trait DataType[I] extends IDataTypeObject {
  override type ScalaClass = I
}

/**
  * The type for all NULL fields
  */
object NoType extends DataType[_] {
  val name: String = "No Such Type!"
}

case class IntType(val name:String = "Int") extends DataType[BigInt] {


  override def fromString(s: String): Option[ScalaClass] = {
    if (s == null) {
      None
    }
    else {
      Some(BigInt(s).intValue)
    }
  }

  override def toString(datum: GeneratedValue[_]): String = {
    datum match {
      case x: TypedValue[BigInt] => x.value match {
        case Some(x: BigInt) => x.toString;
        case _ => "";
      }
      case _ => ""
    }

  }

  override def objectFromString(strRep: String): Option[ScalaClass] = {
    strRep match {
      case s: String => if (s.trim().isEmpty) None else Some(BigInt(strRep));
      case _ => None

    }
  }

}

case class Text(size: Int = -1) extends DataType[String] {
  val name = "Text"

  override def objectFromString(strRep: String): Option[ScalaClass] = fromString(strRep)

  override def fromString(s: String): Option[ScalaClass] = {
    Option(s)
  }

  override def toString(datum: GeneratedValue[_]): String = {
    datum match {
      case x: TypedValue[String] => x.value match {
        case Some(x: String) => x;
        case _ => "";
      }
      case _ => ""
    }

  }

}

case class Decimal(decimalPlaces: Int) extends DataType[BigDecimal] {
  type scalaOptClass = Option[ScalaClass];
  val name = "Decimal"

  override def objectFromString(strRep: String): scalaOptClass = fromString(strRep)

  override def fromString(s: String): scalaOptClass = Some(BigDecimal(s).setScale(decimalPlaces))

  override def toString(datum: GeneratedValue[_]): String = {
    datum match {
      case x: TypedValue[BigDecimal] => x.value match {
        case Some(x: BigDecimal) => x.setScale(decimalPlaces).toString
        case _ => ""
      }
      case _ => ""
    }
  }
}

case object Float extends DataType[BigDecimal] {
  type scalaOptClass = Option[ScalaClass]
  val name = "Float"

  override def objectFromString(strRep: String): scalaOptClass = fromString(strRep)

  override def fromString(s: String): scalaOptClass = {
    Some(BigDecimal(s))
  }

  override def toString(datum: GeneratedValue[_]): String = {
    datum match {
      case x: TypedValue[BigDecimal] => x.value match {
        case Some(x: BigDecimal) => x.toString;
        case _ => "";
      }
      case _ => ""
    }

  };
};

object DateType{
  val format: String = "yyyy/MM/dd hh:mm:ss"
}
case class DateType(val format: String = DateType.format) extends DataType[java.util.Date] {
  type scalaOptClass = Option[java.util.Date]
  val name = "Date"

  def dfmt = new SimpleDateFormat(format)

  override def objectFromString(strRep: String): scalaOptClass = fromString(strRep)

  override def fromString(s: String): scalaOptClass = {
    if (s == null) {
      None
    }
    else {
      Some(dfmt.parse(s))
    }
  }

  override def toString(datum: GeneratedValue[_]): String = {
    datum match {
      case x: TypedValue[DateClass] => x.value match {
        case Some(x: java.util.Date) => dfmt.format(x)
        case _ => ""
      }
      case _ => ""
    }
  }

}

