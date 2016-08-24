package com.objectdynamics.tdg.generators

import com.objectdynamics.tdg.model._
import com.objectdynamics.tdg.spec._
import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.builder._
import com.objectdynamics.tdg.parser.model._
import com.objectdynamics.tdg.util._

/**
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 12/14/10
 * Time: 1:36 AM
 *
 */

class RotateString extends FieldGeneratorFunction with NoLogContributor
{
    def prefix(fldName: String): String =
    {
        name + ":" + fldName + ":"
    }


    def init(ctxt: Ctxt, dataField: IDataField, nRows: Long, fldGenConstraints: Option[FieldGenConstraints]) =
    {
        val getContext = ctxt._2
        val setContext = ctxt._1

        getContext(prefix(dataField.name) + "@initialized") match
        {
            case None =>
                log("RotateString.init - first Time")
                setContext(prefix(dataField.name) + "@initialized", true);

                val data: List[String] = fldGenConstraints match
                {
                    case fgc: FieldGenConstraints =>
                    {
                        fgc.getInSpec match
                        {
                            case Some(InSpec(d: List[String])) => d;
                            case _ =>
                            {

                                dataField.data match
                                {
                                    case Some(l: List[_]) => l.asInstanceOf[List[AnyRef]] map ((x: AnyRef) => x.toString)
                                    case _ => throw new GeneratorException("no strings to rotate")
                                }


                            };
                        }
                    }
                    case _ => dataField.data match
                    {
                        case Some(l: List[_]) => l.asInstanceOf[List[AnyRef]] map ((x: AnyRef) => x.toString)
                        case _ => throw new GeneratorException("no strings to rotate")
                    }
                }
                setContext(prefix(dataField.name) + "data", data);


                dataField.disbursementSpec match
                {
                    case Some(dspec: DisbursementSpec) =>
                        val dsb: Disbursement = new BasicDisbursement(dspec,
                                                                      nRows, 0, 0);
                        setContext(prefix(dataField.name) + "disbursement", dsb);


                    case None => throw new GeneratorException("'disbursementSpec' is required: No disbursementSpec.")
                }
            case _ =>
                log("RotateString.init - NOT first Time")

        }
    }

    def generateField(implicit ctxt: Ctxt, dataField: IDataField, fldGenConstraints: Option[FieldGenConstraints]): ValueFunction =
    {
        val getContext = ctxt.rd
        val setContext = ctxt.wr
        val px = prefix(dataField.name);
        val dsb: BasicDisbursement = getContext(px + "disbursement").get.asInstanceOf[BasicDisbursement];
        //log("RotateString.generateField(): disb:"+dsb.setIdx + "," + dsb.elementIdx)

        setContext(px + "disbursement", dsb.advance);
        val data: List[String] = getContext(px + "data").get.asInstanceOf[List[String]];
        val v = dsb.disburse(data).toString;
        log("RotateString.generateField(): disb:"+dsb.setIdx + "," + dsb.elementIdx + " = "+ v)

        new ValueFunction(TextValue(dataField.name, Some(v)))
    }

    val name: String = "rotatedStrings";


    def canGenerateField(fld: IDataField, fieldConstraints: Option[FieldGenConstraints]): Boolean =
    {
        //log("RotateString.canGenerateField")
        //        fld.isNormal &&
        fld.data != None &&
          (fld.dataType == com.objectdynamics.tdg.spec.datatypes.Text)
    }

}