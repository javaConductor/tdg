package com.objectdynamics.tdg.generators

import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.parser.model._
import com.objectdynamics.tdg.util._

import scala.math.BigDecimal

/**
  * Created by IntelliJ IDEA.
  * User: Lee
  * Date: 12/14/10
  * Time: 1:36 AM
  * To change this template use File | Settings | File Templates.
  */

class GenerateIntegerAutoIncrement extends FieldGeneratorFunction with NoLogContributor {

  val name: String = "autoIncrement";

  def init(ctxt: Ctxt, dataField: IDataField, nRows: Long, fldGenConstraints: Option[FieldGenConstraints]) = {
    val getContext = ctxt.rd;
    val setContext = ctxt.wr;
    val px = prefix(dataField.name + "." + dataField.disbursementSpec);
    if (getContext(px + "lastValue") == None) {
      val (n, l) =
        fldGenConstraints match {

          case Some(fc: FieldGenConstraints) =>

            getMinMax(fc, dataField) match {
              case Some(xx: (BigDecimal, BigDecimal)) => xx;
              case _ => (BigDecimal(1) -> BigDecimal(0));
            }
          case _ => (BigDecimal(1) -> BigDecimal(0));

        };
      setContext(px + "lastValue", n);
      setContext(px + "nRows", nRows);
    }
  }

  def prefix(fldName: String): String = {
    "GenerateIntegerAutoIncrement:" + fldName + ":"
  }

  def generateField(implicit ctxt: Ctxt, dataField: IDataField, fldGenConstraints: Option[FieldGenConstraints]): ValueFunction = {
    val px = prefix(dataField.name + "." + dataField.disbursementSpec);
    val getContext = ctxt.rd;
    val setContext = ctxt.wr;

    val nRows: Int = getContext(px + "nRows") match {
      case Some(a: Any) => a.toString.toInt
      case _ => 1;
    }
    //init(ctxt, dataField, nRows)(ge);

    var v: BigDecimal = getContext(px + "lastValue") match {
      case scala.Some(x: BigDecimal) => x;
      case None => BigDecimal(0);
    }
    //v = v + 1;
    setContext(px + "lastValue", v + 1);
    log("GenerateIntegerAutoIncrement.generateField(): = " + v)
    new ValueFunction(IntValue(dataField.name, Some(BigInt(v.longValue))));
  }

  def canGenerateField(fld: IDataField, fieldConstraints: Option[FieldGenConstraints]): Boolean = {
    // !fld.isAggregate && !fld.isForeignRef &&
    ((fld.dataType == com.objectdynamics.tdg.spec.datatypes.IntType) &&
      (fieldConstraints match {
        case Some(fc: FieldGenConstraints) => (fc.getInSpec == None)
        case _ => true;
      }))

  }

  // override def apply(implicit ctxt: Ctxt, field: IDataField, fldGenConstraints: Option[FieldGenConstraints]): ValueFunction = null
}