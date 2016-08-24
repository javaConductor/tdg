package com.objectdynamics.tdg.generators

import scala.Option
import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.builder._
import com.objectdynamics.tdg.parser.model._

object defaultTreeRequest
  extends TreeRequest(null, 0L, Map[String, FieldGenConstraints](), None, None, List[TreeRequest](), false);

trait FieldGeneratorFunction
//       extends Function3[Ctxt,
//  IDataField, Option[FieldGenConstraints], ValueFunction]
{
    implicit val treeRequest: TreeRequest = defaultTreeRequest;

    def apply(implicit ctxt: Ctxt, field: IDataField, fldGenConstraints: Option[FieldGenConstraints]): ValueFunction = generateField(ctxt, field, fldGenConstraints)

    //    override def apply(implicit ctxt: Ctxt,
    //                     field: IDataField,
    //                     fldGenConstraints: Option[FieldGenConstraints] = None):ValueFunction = {
    //    generateField(ctxt, field, fldGenConstraints)
    //    }

    /*             generateField
    These methods (and values) are abstract
    */
    def canGenerateField(fld: IDataField, fieldConstraints: Option[FieldGenConstraints]): Boolean;
    val name: String;


    def generateField(implicit ctxt: Ctxt,
                      dataField: IDataField,
                      fldGenConstraints: Option[FieldGenConstraints] = None): ValueFunction;

    def init(ctxt: Ctxt,
             dataField: IDataField,
             nRows: Long,
             fldGenConstraints: Option[FieldGenConstraints]);


    def getMinMax(fldGenConstraints: FieldGenConstraints, dataField: IDataField): Option[(BigDecimal, BigDecimal)] =
    {


        fldGenConstraints.getBetweenSpec match
        {

            case Some(x: BetweenSpec) => Some(BigDecimal(x.min) -> BigDecimal(x.max))
            case _ =>
            {
                fldGenConstraints.getEqSpec match
                {
                    case Some(EqSpec(value: String)) => Some(BigDecimal(value) -> BigDecimal(value))
                    case _ => if(!dataField.min.isEmpty && !dataField.max.isEmpty)
                                  Some(dataField.min.get -> dataField.max.get)
                              else None;
                }
            }
        }
    }
}

