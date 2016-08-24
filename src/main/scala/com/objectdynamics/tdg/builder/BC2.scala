package com.objectdynamics.tdg.builder

import com.objectdynamics.tdg.delivery.DeliveryAgent
import com.objectdynamics.tdg.asyncmessages._

import com.objectdynamics.tdg.builder.model._
import scala.concurrent.Future
import scala.concurrent.stm._
import scala.concurrent.stm.Ref
import com.objectdynamics.tdg.builder.rdbms._
import com.objectdynamics.tdg.util._
import java.util.Calendar
import com.objectdynamics.tdg.model.TestData


//import collection.mutable.HashMap

import akka.actor._
import com.objectdynamics.tdg.spec._;
import com.objectdynamics.tdg.parser.model._

object BC2
{

}

class BC2(val schema: ITestDataSchema,
          val agent: DeliveryAgent,
          val request: BuildRequest,
           val f:(ITestData => Unit))
  extends AbstractBuildCoordinator

  with Actor
   with ListUtil
  with BuilderContextSupport
  with TreeValidator
  with TreeRequestBuilder
  with SpecificsGenerator
{

    val state = Ref(new BuildCoordinatorState(f));

    def receive = {
            case init: InitBuildCoordinator =>

                atomic
                {
                    implicit txn => // nested atomic
                        state.transform((bcs: BuildCoordinatorState) => bcs.withAgent(init.agent)
                                                                        .withBuildRequest(init.buildRequest));
                    null;
                }


            case StartBuild  =>
                startBuild(); null;

            //handleIt(bcState.copy(builders = b));

            case nb: NewBuilder =>
                log("BuildCoordinator: msg:NewBuilder -> [" + nb + "]");
                atomic
                {
                    implicit txn => // nested atomic
                        state.transform((bcs: BuildCoordinatorState) => bcs.withBuilders((nb.b :: bcs.builders).distinct));
                    null;

                }
            //handleIt(bcState.copy((nb.b :: bcState.builders).distinct));

            case objReq: ForeignObjectFunctionRequest =>
                log("BuildCoordinator: msg -> " + objReq + ") from BC ");
                val bMap: Map[String, IDataSetBuilder] = (state.single.get.builders map ((b: IDataSetBuilder) => (b.dataSetSpec.name -> b))).toMap;
                onForeignObjectFunctionRequest(bMap, objReq, objReq.client);
                null;


            case objReq: ForeignObjectFunctionBCRequest =>
                val bMap: Map[String, IDataSetBuilder] = (state.single.get.builders map ((b: IDataSetBuilder) => (b.dataSetSpec.name -> b))).toMap;
                log("BuildCoordinator: msg -> " + objReq + ") from BC ");
                onForeignObjectFunctionRequest(bMap, objReq, objReq.client);
                null;

                      // handleIt(bcState.copy(builders = (bcState.builders ::: bl).distinct));

            case gr: GeneratedRow => null;
            //logActivity(gr);
            //handleIt(bcState);

            case endDs: EndDataSet =>
            {
                log("BuildCoordinator: msg -> (" + endDs + ") from Builder ");

                val completeSets = (endDs.dataSet.name :: state.single.get.completeDataSets).distinct;

                log("BuildCoordinator: msg -> (" + endDs + ") completeSets=" + completeSets);
               // val nuState = state.single.get.copy(completeDataSets = completeSets);
                atomic
                {
                    implicit txn => // nested atomic
                        state.transform((bcs: BuildCoordinatorState) => bcs.withCompletedDataSets(completeSets) withTestData bcs.testData.get + endDs.dataSet);
                    null;

                }

                if(state.single.get.dataSetsComplete)
                {
                    log("BuildCoordinator: msg -> (" + endDs + ") ALL DataSets Completed! ");
                    this ! DataSetsCompleted;
                }
                null
            }

            case DataSetsCompleted =>
                log("BuildCoordinator: msg -> (" + DataSetsCompleted + ") from self.");

                // here is where we process the aggregates
                state.single.get.buildRequest match
                {
                    case Some(br: BuildRequest) =>
                        val (d: Map[String, List[IDataRow]], st: SymbolTable) = generateSpecifics(br, schema, new
                            SymbolTable, state.single.get);
                        atomic
                        {
                            implicit txn => // nested atomic
                                state.transform((bcs: BuildCoordinatorState) => bcs.withSymbolTable(st));
                            null;

                        }
                    // not storing data - it is kept with the builder

                    case _ => null
                }

                state.single.get.builders foreach ( _ ! Stop)
                this.agent ! Stop;

                for ( f <- state.single.get.f ; t <-  state.single.get.testData)  yield  {
                    log("BuildCoordinator: msg -> (" + DataSetsCompleted + ") for b,t");
                    f( t )
                }
                exit();


            /// capture the Specifics data here
            case d: PostCompleteData =>
                // if configured, add data to the BC's local copy of the dataSets
                //
                //handleIt(bcState);
                null;

            case x: AnyRef =>
            {
                log("BuildCoordinator(): bad msg:[" + x.getClass + "]=" + x.toString);
                //handleIt(bcState);
                null
            }
            case x: Any =>
            {
                log("BuildCoordinator(): bad msg:" + x.toString)
                // handleIt(bcState);
                null
            }
        }


    def onForeignObjectFunctionRequest(builderMap: Map[String, IDataSetBuilder], objReq: ForeignObjectFunctionBCRequest, sender:OutputChannel[Any]): List[IDataSetBuilder] =
     {
          log("BuildCoordinator.onForeignObjectFunctionRequest: " + objReq);
         val tr = mainTreeRequest(objReq.dataSetName,objReq.nRows,schema.dataSetSpec(objReq.dataSetName).get)   ;

         //this ! new ForeignObjectFunctionRequest(tr, objReq)  ;
         onForeignObjectFunctionRequest(builderMap,new ForeignObjectFunctionRequest(tr, objReq), objReq.client) ;
     }

    /**
     * case class ForeignObjectFunctionRequest(requestId: Long,
     * treeRequest: TreeRequest,
     * fieldsToGenerate: List[IDataField],
     * preDefValues: Map[String, GeneratedValue],
     * nRows: Long) extends NativeMessage;
     */

    def onForeignObjectFunctionRequest(builderMap: Map[String, IDataSetBuilder], objReq: ForeignObjectFunctionRequest, sender:OutputChannel[Any]): List[IDataSetBuilder] =
    {
        log("BuildCoordinator.onForeignObjectFunctionRequest: " + objReq);

        state.single.get.builders.find((b: IDataSetBuilder) => b.dataSetSpec.name.equals(objReq.treeRequest.dataSetName))
        match
        {

            //if it exists then use it
            case Some(bldr: IDataSetBuilder) =>
            {
                log("BuildCoordinator.onForeignObjectFunctionRequest: sending " + objReq + " to builder:" + bldr);
                bldr.forward(objReq);
                Nil

            }
            // if it does not exist then create it and init it
            case None =>
            {
                log("BuildCoordinator.onForeignObjectFunctionRequest: creating builder for:" + objReq.treeRequest.dataSetName);

                val bldr: IDataSetBuilder = createAndInitTableBuilder(this,
                                                                      schema,
                                                                      objReq.treeRequest,
                                                                      agent,
                                                                      0);

                log("BuildCoordinator.onForeignObjectFunctionRequest: builder created  for:" + bldr);
                Thread.sleep(100);
                // forward request to new builder
                log("BuildCoordinator.onForeignObjectFunctionRequest: sending " + objReq + " to builder:" + bldr);
                bldr.forward(objReq);

                // give it a sec
                Thread.sleep(500)
                /// start
                bldr ! com.objectdynamics.tdg.asyncmessages.StartDataSetBuild;
                // send note self about the newly created builder

                this ! NewBuilder(bldr);
                Nil


            }
        }

    }


    def relationships(tr: TreeRequest): Map[String, List[Relationship]] =
    {
        val ret: Map[String, List[Relationship]] = (tr.subTrees map
          {
              (stree: TreeRequest) =>
              // look at each of the fields for a relationship to tr.ds
              // for each field fld ->
                  val dss = schema.dataSetSpec(stree.dataSetName).get
                  //  add the relationship to the list
                  val l: List[Relationship] = dss.relationships filter
                    {
                        (rl: Relationship) =>
                            rl.dataSetName == tr.dataSetName
                    }
                  //  yield ( fld.name, list)
                  (stree.dataSetName -> l)
          } toMap).asInstanceOf[Map[String, List[Relationship]]];
        return ret;
    }

    def relationship(tr: TreeRequest, dsName: String): List[Relationship] =
    {
        relationships(tr).get(dsName) match
        {
            case rlist: List[Relationship] => rlist;
            case _ => Nil;
        }
    }

    //why am I waiting for the data when the builders can keep it ??????
    /////// I guess the caller can either use the value or throw it away
    /////// or use it as 'proof'

    def createRequestId(request: TreeRequest): String  =   {

        request.dataSetName +
          ( if (request.hasSymbolicName) request.symbolicName else "" ) +
        ( if (request.hasSymbolicName) request.symbolicName else "" ) +
        "."+Calendar.getInstance().getTime;


    }

    def sendForeignObjectRequest_Wait(dsName: String,
                                      bldr: IDataSetBuilder,
                                      treeReq: TreeRequest,
                                      // fldGenConstraints: Map[String,FieldGenConstraints],
                                      fields: Map[String,String]): List[IDataRow] =
    {
        val dss = schema.dataSetSpec(dsName) get;
        //var dr: IDataRow = new DataRow(dss);
        val prReq = ObjectRequest(createRequestId(treeReq), treeReq,
                                  bldr,
                                  fields ,//map ((fldName: String) => dss.field(fldName).get),
                                  Map.empty,
                                  1L);
        //    val prReq = PartialRowRequest(-1, fldGenConstraints, fields map ((fldName: String) => dss.field(fldName).get), 1);
        log("BuildCoordinator.sendForeignObjectRequest(): sending foreign obj req and WAITING!:" + treeReq.dataSetName);
        val ftr: Future[List[IDataRow]] = (bldr !! prReq).asInstanceOf[Future[List[IDataRow]]];
        val dr = ftr();
        log("BuildCoordinator.sendForeignObjectRequest(): Got IT!" + treeReq.dataSetName + "->" + dr);
        dr
    }

    def sendForeignObjectRequest(dsName: String,
                                 bldr: IDataSetBuilder,
                                 treeReq: TreeRequest,
                                 // fldGenConstraints: Map[String,FieldGenConstraints],
                                 fields: Map[String,String]) =
    {
        val dss = schema.dataSetSpec(dsName) get;
        //var dr: IDataRow = new DataRow(dss);
        val prReq = ObjectRequest(createRequestId(treeReq), treeReq,
                                  bldr,
                                  fields,
                                  Map.empty,
                                  1L);
        log("BuildCoordinator.sendForeignObjectRequest(): sending foreign obj " + prReq + " req:" + treeReq.dataSetName);

        (bldr ! prReq);
    }

    def newBuilder(dss: IDataSetSpec, bc: IBuildCoordinator): IDataSetBuilder = createBuilder(bc, this.agent, dss);


    def logActivity(gr: GeneratedRow) =
    {
        log(gr.toString);
    }

    /**
     * This method is called to begin the building of test data.
     *
     */
    def startBuild() =
    {
        log("BuildCoordinator.startBuild( )");
        startTreeRequest(mainTreeRequest(request.rootObject, request.nRows, schema.dataSetSpec(request.rootObject).get))
        request.specifics foreach
          {
              (t: TreeRequest) =>
                  log("BuildCoordinator.startBuild(): validating request.");
                  if(validateTreeRequest(schema, t))
                      startTreeRequest(t);

          };

        // mainBldr :: builders;
    }


    def startTreeRequest(treq: TreeRequest) =
    {

        log("BuildCoordinator.startTreeRequest(): create and init:" + treq.dataSetName);
        val bldr: IDataSetBuilder = createAndInitTableBuilder(this, schema, treq, agent, treq.rows)
        //builders += (request.rootObject -> bldr);
        log("BuildCoordinator.startBuild(): sending start to:" + bldr);
        // Start the builder
        bldr ! com.objectdynamics.tdg.asyncmessages.StartDataSetBuild;
        log("BuildCoordinator.startBuild(): sending NewBuilder to self.");
        this ! NewBuilder(bldr);

    }


    private def createAndInitTableBuilder(bc: BC2,
                                          schema: ITestDataSchema,
                                          treq: TreeRequest,
                                          agent: DeliveryAgent,
                                          nRows: Long): IDataSetBuilder =
    {
        val dataSetName: String = treq.dataSetName;
        log("BuildCoordinator.createAndInitTableBuilder(" + dataSetName + "," + nRows + ")");
        val tb: IDataSetBuilder = createBuilder(this, agent, schema.dataSetSpec(dataSetName).get)
        tb.start;
        tb ! InitBuilder(nRows, treq);
        tb;
    }

    /**
     * Must be changed to use the datasetType to pick the Builder
     */
    private def createBuilder(bc: IBuildCoordinator, agent: DeliveryAgent, dss: IDataSetSpec): IDataSetBuilder =
    {
        log("BuildCoordinator.createBuilder(" + dss.name + "): found " +
              dss);
        val tb = new TableBuilder2(this, dss, agent);
        tb;
    }

}

//end BC3 class
