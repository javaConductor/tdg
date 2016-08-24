package com.objectdynamics.tdg.spec.datatypes

import java.text.SimpleDateFormat
import com.objectdynamics.tdg.builder._
import com.objectdynamics.tdg.generators._

/**
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: Nov 19, 2010
 * Time: 1:22:10 AM

 */

object DataTypeObject extends IDataTypeObject
{

    def fromString(dtype: String, size: Int): DataType =
    {
        dtype match
        {
            case ("Int") => IntType;
            case ("Integer") => IntType;
            case ("Date") => DateType;
            case "Decimal" => Decimal(size);
            case ("Float") => Float;
            case ("Text") => Text;
            case ("String") => Text;
            case s: String => throw new BuilderException("No such DataType:" + s);
        }
    }

    override def fromString(dtype: String): IDataTypeInstance = fromString(dtype, 0);

    def fromOption(dtype: Option[String]): IDataTypeInstance =
    {
        dtype match
        {
            case Some(s: String) =>
                //TODO take the '(#)' from off the end, if any
                fromString(s);
            case _ => throw new BuilderException("No Data Type")
        }
    }

    def typeMap(): Map[String, DataType] =
    {
        var m: Map[String, DataType] = Map.empty;

        m = m + ("Int" -> IntType);
        m = m + ("Date" -> DateType);
        m = m + ("Decimal" -> Decimal(3));
        m = m + ("Float" -> Float);
        m = m + ("Text" -> Text);
        m
    }

}


trait NormalDataType extends IDataTypeInstance
{
    type JDate = java.util.Date;
}

sealed trait DataType extends NormalDataType
{
    type scalaClass;
}

/**
 * The type for all NULL fields
 */
object NoType extends DataType
{

    type scalaClass = Any;

    def toString(datum: GeneratedValue): String =
    {

        datum.value.toString;

    };

    def objectFromString(strRep: String): Option[scalaClass] = None;

    val name: String = "No Such Type!"

    def fromString(s: String): Option[_] = None;
}

case object IntType extends DataType
{
    val name = "Int";
    type scalaClass = BigInt;

    def fromString(s: String): Option[_] =
    {
        if(s == null)
        {
            None
        }
        else
        {
            Some(scala.BigInt(s).intValue)
        }
    }

    def toString(datum: GeneratedValue): String =
    {
        datum match
        {
            case x: TypedValue[BigInt] => x.value match
            {
                case Some(x: BigInt) => x.toString;
                case _ => "";
            }
            case _ => ""
        }

    };


    def objectFromString(strRep: String): Option[BigInt] =
    {
        strRep match
        {


            case s: String => if(s.trim().isEmpty) None else Some(BigInt(strRep));
            case _ => None

        }
    }

    def generatedValueFromString(strRep: String): GeneratedValue =
    {
        strRep match
        {
            case s: String if (s.trim().isEmpty) => NullValue
            case s: String => new AnonymousIntValue(Some(BigInt(s)))
        }

    }

};

case object Text extends DataType
{
    val name = "Text";
    type scalaClass = String;

    def fromString(s: String): Option[scalaClass] =
    {
        if(s == null) None else Some(s);
    }


    def objectFromString(strRep: String): Option[Text.scalaClass] = fromString(strRep)

    def toString(datum: GeneratedValue): String =
    {
        datum match
        {
            case x: TypedValue[String] => x.value match
            {
                case Some(x: String) => x;
                case _ => "";
            }
            case _ => ""
        }

    };
};

case class Decimal(decimalPlaces: Int) extends DataType
{
    val name = "Decimal";
    type scalaClass = BigDecimal;
    type scalaOptClass = Option[scalaClass];

    def fromString(s: String): scalaOptClass =
    {
        var bd = BigDecimal(s).setScale(decimalPlaces);
        Some(bd)
    }

    def objectFromString(strRep: String): scalaOptClass = fromString(strRep)

    def toString(datum: GeneratedValue): String =
    {
        datum match
        {
            case x: TypedValue[BigDecimal] => x.value match
            {
                case Some(x: BigDecimal) => x.setScale(decimalPlaces).toString;
                case _ => "";
            }
            case _ => ""
        }

    };

};

case object Float extends DataType
{
    val name = "Float";
    type scalaClass = BigDecimal;
    type scalaOptClass = Option[scalaClass];

    def objectFromString(strRep: String): scalaOptClass = fromString(strRep)

    def fromString(s: String): scalaOptClass =
    {
        var bd = BigDecimal(s);
        Some(bd)
    }


    def toString(datum: GeneratedValue): String =
    {
        datum match
        {
            case x: TypedValue[BigDecimal] => x.value match
            {
                case Some(x: BigDecimal) => x.toString;
                case _ => "";
            }
            case _ => ""
        }

    };
};

case object DateType extends DataType
{
    val name = "Date";
    val dfmt = new SimpleDateFormat("yyyy/MM/dd hh:mm:ss");
    type scalaClass = java.util.Date;
    type scalaOptClass = Option[scalaClass];

    //    val tfmt = new SimpleDateFormat("yyyyMMdd");
    def objectFromString(strRep: String): scalaOptClass = fromString(strRep)

    def fromString(s: String): scalaOptClass =
    {
        if(s == null)
        {
            None
        }
        else
        {
            Some(dfmt.parse(s))
        }
    }


    def toString(datum: GeneratedValue): String =
    {
        datum match
        {
            case x: TypedValue[JDate] => x.value match
            {
                case Some(x: java.util.Date) => dfmt.format(x);
                case _ => "";
            }
            case _ => ""
        }
    }

};

