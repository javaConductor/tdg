package com.objectdynamics.tdg.generators

import java.util.Date
import com.objectdynamics.tdg.spec.datatypes._
import com.objectdynamics.tdg.builder.ValueFunction

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
trait GeneratedValue
{
    def dataType: DataType;

    def value: Any;

};

trait TypedValue[A] extends GeneratedValue
{
    def value: Option[A];

    def apply() = value;
};

trait NamedGeneratedValue extends GeneratedValue
{
    def name: String;
};

trait NamedTypedGeneratedValue[A] extends TypedValue[A] with NamedGeneratedValue
{


    def apply[A](nm: String, v: TypedValue[A]): NamedTypedGeneratedValue[A] =
    {

        new NamedTypedGeneratedValue[A]
        {
            def name: String = nm;

            def dataType: DataType = v.dataType;

            def value: Option[A] = v.value;
        }
    }
};

case class AnonymousNumberValue(value: Option[Float.scalaClass]) extends TypedValue[BigDecimal]
{
    def dataType: DataType = Float;
}

case class AnonymousIntValue(value: Option[IntType.scalaClass]) extends TypedValue[BigInt]
{
    def dataType: DataType = IntType;
}

case class AnonymousTextValue(value: Option[Text.scalaClass]) extends TypedValue[String]
{
    def dataType: DataType = Text;
}

case class AnonymousDecimalValue(value: Option[BigDecimal], decimalPlaces: Int)
  extends TypedValue[BigDecimal]
{
    def dataType: DataType = Decimal(decimalPlaces);
}

case class AnonymousFloatValue(value: Option[BigDecimal]) extends TypedValue[BigDecimal]
{
    def dataType: DataType = Float;

}

case class AnonymousDateValue(value: Option[Date]) extends TypedValue[Date]
{
    def dataType: DataType = DateType;
}

case class NumberValue(name: String, value: Option[BigDecimal]) extends NamedTypedGeneratedValue[BigDecimal]
{
    def dataType: DataType = Float;
}

case class IntValue(name: String, value: Option[BigInt]) extends NamedTypedGeneratedValue[BigInt]
{
    def dataType: DataType = IntType;
}

case class TextValue(name: String, value: Option[String]) extends NamedTypedGeneratedValue[String]
{
    def dataType: DataType = Text;
}

case class DecimalValue(name: String, value: Option[BigDecimal], decimalPlaces: Int)
  extends NamedTypedGeneratedValue[BigDecimal]
{
    def dataType: DataType = Decimal(decimalPlaces);
}

case class FloatValue(name: String, value: Option[BigDecimal]) extends NamedTypedGeneratedValue[BigDecimal]
{
    def dataType: DataType = Float;

}

case class DateValue(name: String, value: Option[Date]) extends NamedTypedGeneratedValue[Date]
{
    def dataType: DataType = DateType;
}

object NullValue extends NamedTypedGeneratedValue[Nothing]
{
    def dataType: DataType = NoType;

    def value = None;

    def name: String = "<NULL>"
}


class valueFunction(gval: GeneratedValue) extends Function0[GeneratedValue]
{
    override
    def apply(): GeneratedValue =
    {
        gval
    }
}

object nullObjectFunction extends ValueFunction
{
    override
    def apply() =
    {
        NullValue
    }
}