package com.objectdynamics.tdg.generators

import scala.util.Random
import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.builder._
import com.objectdynamics.tdg.parser.model.FieldGenConstraints
import com.objectdynamics.tdg.util._
;

/**
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 12/14/10
 * Time: 1:36 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class GenerateRandomNumber extends FieldGeneratorFunction with NoLogContributor
{
    def prefix(fldName: String): String =
    {
        name + ":" + fldName + ":"
    }


    def init(ctxt: Ctxt, dataField: IDataField, nRows: Long, fldGenConstraints: Option[FieldGenConstraints]) =
    {
        val getContext = ctxt._2
        val setContext = ctxt._1
        val px = prefix(dataField.name)
        getContext(px + "@initialized") match
        {
            case None =>
                log("GenerateRandomNumber.init - first Time");
                ctxt._1.apply(px + "@initialized", true);

                val (min, max) = fldGenConstraints match
                {
                    case Some(fgc: FieldGenConstraints) => this.getMinMax(fgc, dataField) match
                    {
                        case Some(x: (BigDecimal, BigDecimal)) => x;
                        case _ => (BigDecimal(0) -> BigDecimal(0))
                    }
                    case _ => (BigDecimal(0) -> BigDecimal(0))
                }


                //val prefix:String = "GenerateRandomNumber:"+dataField.name+":";
                val rand: Random = new Random(new java.util.Date().getTime)

                //val getContext = ctxt._2
                val setContext = ctxt._1
                setContext(px + "min", min);
                setContext(px + "max", max);
                setContext(px + "rand", rand);
            case _ =>

        }
    }


    def generateField(implicit ctxt: Ctxt, dataField: IDataField, fldGenConstraints: Option[FieldGenConstraints]): ValueFunction =
    {
        //val setContext = ctxt.wr;
        val getContext = ctxt.rd;
        val v = (
          if(dataField.unique)
          {
              throw new GeneratorException("No support for UNIQUE fields as yet... workin' on it. ")
              //getUniqueValue(getContext, withRows, dataField.name)
          }
          else
          {
              getValue(getContext, dataField.name)
          }
          );
        new ValueFunction(v);
    }



    def isUniq(value: GeneratedValue, rows: List[IDataRow], fldName: String): Boolean =
    {

        def columnList: List[ValueFunction] =
            rows map
              {
                  obj: IDataRow => obj.value(fldName)
              }

        val b: Boolean = columnList forall
          {
              columnVal: ValueFunction => columnVal.gv match
              {
                  case x: IntValue => !x.value.equals(value.value);
                  case _ => true
              }

          }
        b;
    }

    def getValue(getContext: (String => Option[Any]), fldName: String): GeneratedValue =
    {
        val min: BigDecimal = getContext(prefix(fldName) + "min") match
        {
            case Some(a: Any) => BigDecimal(a.toString);
            case _ => BigDecimal(1);
        }

        val max: BigDecimal = getContext(prefix(fldName) + "max") match
        {
            case Some(a: Any) => a.asInstanceOf[BigDecimal];
            case None => BigDecimal(Double.MaxValue);
        }
        val rand: Random = getContext(prefix(fldName) + "rand") match
        {
            case Some(a: Random) => a //.asInstanceOf[Random];
            case _ => Random;
        }

        val bd = BigDecimal.valueOf(min.doubleValue + (rand.nextDouble * (max.doubleValue - min.doubleValue)));
        NumberValue(fldName, Some(bd));
    }

    val name: String = "randomNumber";


    def canGenerateField(fld: IDataField, fieldConstraints: Option[FieldGenConstraints]): Boolean;

}

class RandomInteger extends GenerateRandomNumber
{

    override def generateField(implicit ctxt: Ctxt, dataField: IDataField, fldGenConstraints: Option[FieldGenConstraints]): ValueFunction =
    {
        new ValueFunction(
            super.generateField(ctxt, dataField, fldGenConstraints) match
            {
                case ival: IntValue => ival;
                case gval: NumberValue => gval.value match
                {
                    case Some(bd: BigInt) =>
                        IntValue(dataField.name, Some(bd));

                    case None => NullValue
                }
            });

    }

    override val name: String = "randomInteger";

    override def canGenerateField(fld: IDataField, fieldConstraints: Option[FieldGenConstraints]): Boolean =
    {
        (fld.dataType == com.objectdynamics.tdg.spec.datatypes.IntType)
    }

}

class RandomFloat extends GenerateRandomNumber
{
    override val name: String = "randomFloat";

    override def canGenerateField(fld: IDataField, fieldConstraints: Option[FieldGenConstraints]): Boolean =
    {
        (fld.dataType == com.objectdynamics.tdg.spec.datatypes.Float)
    }

}