package com.objectdynamics.tdg.builder.rdbms

import com.objectdynamics.tdg.parser.model.TreeRequest
import scala.actors.Actor
import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.spec.Relationship
import com.objectdynamics.tdg.builder._
import com.objectdynamics.tdg.model._
import scala.annotation.tailrec
import com.objectdynamics.tdg.asyncmessages._
import scala.concurrent.SyncVar
import com.objectdynamics.tdg.generators.GeneratedValue
import scala.concurrent.stm.atomic

/**
 * Created by Object Dynamics Inc.
 *
 * User: lcollins
 * Date: 11/2/11
 * Time: 12:28 AM

 * Object Dynamics Inc. (c) 2001 - 2011
 *
 *     Just add code => 
 */

trait ForeignFieldFunks
{

    self: TableBuilder2 =>

    // ///////// REQUESTING FOREIGN DATA // ///////// REQUESTING FOREIGN DATA // ///////////
    // ///////// REQUESTING FOREIGN DATA // ///////// REQUESTING FOREIGN DATA // ///////////
    // ///////// REQUESTING FOREIGN DATA // ///////// REQUESTING FOREIGN DATA // ///////////


    def generateNormalFieldGenFunks(treq: TreeRequest, n: Long, sender: Actor, dataSetSpec: IDataSetSpec): Map[String, ObjectFieldGenFunc] =
    {


        nRows = n.asInstanceOf[Int];

        val normalFlds: List[IDataField] = dataSetSpec.fields filter ((fd: IDataField) => this.dataSetSpec.isNormal(fd.name))

        val normalFunks: List[(String, ObjectFieldGenFunc)] = normalFlds map
          {
              (df: IDataField) => ((df.name -> new
                  LocalGenFunction(df, fldGenList, Some(treq.fieldConstraints(df.name)), n, this.ctxt)))
          }
        log("TableBuilder(" + dataSetName + ").generateNormalFieldGenFunks() created normal funks");

        //// LOOP thru the relationships  and using getForeignFunks2
        //// LOOP thru the relationships  and using getForeignFunks2
        //// LOOP thru the relationships  and using getForeignFunks2





        normalFunks.toMap

    }

    def sendForeignFunkRequest(sender: Actor)
    {
        val foreignFlds: List[IDataField] = dataSetSpec.fields filter ((fd: IDataField) => this.dataSetSpec.isForeignRef(fd.name))
        log("TableBuilder(" + dataSetName + ").generateNormalFieldGenFunks() retrieving funks for fields:" + foreignFlds);

        atomic
        {
            implicit txn => // nested atomic
                state.transform((bs: BuilderState) => bs withMachineState ForeignFieldFunctionRequestPending);
            null;
        }
        doForeign(sender, (foreignFlds map ((df: IDataField) => df.name)), dss.relationships, Map.empty);
    }


    def valueFuncToObjectFieldGenFunc(vf: Map[String, List[ValueFunction]], nRows: Long): Map[String, ObjectFieldGenFunc] =
    {

        val foreignFunks: Map[String, ObjectFieldGenFunc] = vf map
          {
              //            (k: String, v: List[ValueFunction]) =>
              (entry: Tuple2[String, List[ValueFunction]]) =>
                  val k: String = entry._1;
                  val v: List[ValueFunction] = entry._2;

                  val disb = disbursement(relationship(k), nRows) match
                  {
                      case dd: Disbursement => dd;
                      case _ => roundRobinDisbursement(nRows, 0);
                  }

                  (k -> ForeignGenFunction(k, v, disb));
          }
        foreignFunks;
    }


    @tailrec
    private def doForeign(bc: Actor, flds: List[String], rels: List[Relationship], funks: Map[String, List[ValueFunction]]): Unit =
    {

        rels match
        {
            case Nil =>
                log("TableBuilder(" + dataSetName + ").doForeign() returning:" + funks);
                funks;
            case rel :: relList =>
            {
                val n = disbursement(rel, nRows) match
                {
                    case Some(dd: Disbursement) => dd.disbursementSpec.dataItemsRequired(nRows);
                    case _ => throw new BuilderException("Relationship must have disbursement. fields:" + flds)
                }

                val nuFunks: Map[String, List[ValueFunction]] = getValueFunksForRelationship(rel, n, bc);
                val nuFlds = flds flatMap ((fld: String) => if(rel.fieldMap.values.contains(fld)) List(fld) else Nil)

                doForeign(bc, nuFlds, relList, funks ++ nuFunks)
            }
        }
    }

    def getValueFunksForRelationship(rel: Relationship, nRows: Long, client: Actor): Map[String, List[ValueFunction]] =
    {
        //buildCoordinator.
        log("TableBuilder(" + dataSetName + ").getForeignFunks(" + rel + ") ");

        val funcObjReq = ForeignObjectFunctionBCRequest(createReqId(rel.dataSetName, rel.fieldMap.values.toList),
                                                        this,
                                                        rel.dataSetName,
                                                        rel.fieldMap,
                                                        Map.empty,
                                                        nRows);
        log("TableBuilder(" + dataSetName + ").getForeignFunks(" + rel + ") sending fofr:" + funcObjReq + " to BC");


        atomic
        {
            implicit txn => // nested atomic
                state.transform((bs: BuilderState) => bs withNewRequestId (funcObjReq.requestId, funcObjReq));
            null;
        }
        client ! funcObjReq;
        Map.empty

    }

    // ///////// SENDING REQUESTED DATA // ///////// SENDING REQUESTED DATA // ///////////
    // ///////// SENDING REQUESTED DATA // ///////// SENDING REQUESTED DATA // ///////////
    // ///////// SENDING REQUESTED DATA // ///////// SENDING REQUESTED DATA // ///////////
    def sendRequestedData(reqId:String, nRows: Long, fieldsToGenerate: List[String], target: Actor) =
    {

        // check to see if there are already enough withRows to handle the request.

        //val existingRows:List[IDataRow] =findRows(req.treeRequest.fieldConstraints);
        // not going to do that unless necessary later

        val rows: List[IDataRow] = createDataRows(nRows);
        val flds2Gen: List[String] = if(fieldsToGenerate.isEmpty) dataSetSpec.fieldNames else (fieldsToGenerate);

        /**
         * This function, when invoked with (), will get a reference to a generated object via the
         * SyncVar 'sv'
         *
         */
        case class objFunk(sv: SyncVar[IDataRow], obj: IDataRow) extends DataObjectFunction
        {
            def apply() = sv.get;
        }


        /**
         * This function, when invoked with (), will invoke an objFunk instance
         * to get a reference to a generated object and from it get
         * the field 'fieldName' and return the GeneratedValue
         */
        case class fldFunk(fieldName: String, of: objFunk) extends ValueFunction
        {
            override
            def apply() =
            {
                val ff: () => GeneratedValue = of().value(fieldName);
                ff();
            }
        }

        log("TableBuilder(" + dataSetName + ").sendRequestedData() creating functions.");
        /// create the return map
        val retFuncMap: Map[String, List[ValueFunction]] =
            rows.foldRight(Map[String, List[ValueFunction]]())
            {
                (o: IDataRow, retMap: Map[String, List[ValueFunction]]) =>
                    val syncVar: SyncVar[IDataRow] = new SyncVar[IDataRow];

                    // function to be used as post-processing for the newly generated object
                    val f =
                    {
                        (x: IDataRow) =>
                        {
                            log("TableBuilder(" + dataSetName + ").foreignPostProcess(" + x.id + ")");
                            syncVar.set(x);
                            x;
                        }
                    }; //f

                    //create the ObjGen and add it to the queue
                    state.single.get.theQ.get ! ObjectGeneration(o, o.dss.fieldNames, Map.empty, syncVar, Some(f));
                    // create the DataObjectFunction that will return an IDataRow when it is ready
                    val objFunc = objFunk(syncVar, o);

                    // returning the value of this fold to the outer fold
                    flds2Gen.foldRight(retMap)
                    {
                        (fldName: String, accMap: Map[String, List[ValueFunction]]) =>
                        // create a field function to return the ValueFunction for
                        // that field using the DataObjectFunction created above
                            val fldFunc = fldFunk(fldName, objFunc);
                            //add it to the accumulator map
                            accMap.updated(fldName, fldFunc :: (accMap.get(fldName) getOrElse (Nil)));
                    } //fold  "find a place to celebaate the law  - keep on moving" female vocal
            } //fold
        log("TableBuilder(" + dataSetName + ").sendRequestedData() creating functions:" + retFuncMap);

        target ! ForeignObjectFunctionResponse(reqId, retFuncMap);
        retFuncMap;
    }

}