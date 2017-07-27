package com.objectdynamics.tdg.builder.rdbms

import java.util.Calendar

import com.objectdynamics.tdg.asyncmessages._
import com.objectdynamics.tdg.builder._
import com.objectdynamics.tdg.builder.delegate.RequestDelegate
import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.delivery.DeliveryAgent
import com.objectdynamics.tdg.generators._
import com.objectdynamics.tdg.model._
import com.objectdynamics.tdg.parser.model._
import com.objectdynamics.tdg.spec._
import com.objectdynamics.tdg.util._

import scala.actors.Actor
import scala.annotation.tailrec
import scala.concurrent.SyncVar
import scala.concurrent.stm.{Ref, _}

//import scala.io.BytePickle.Ref

//import scala.io.BytePickle.Ref

;

//import com.objectdynamics.tdg.asyncmessages.{RequestedData, RowValueRequest}


class TableBuilder2(val bc: IBuildCoordinator, val dss: IDataSetSpec, val agent: DeliveryAgent)
  extends Builder
    with BuilderContextSupport
    with NoLogContributor
    with TreeRequestBuilder
    with ListUtil
    with ForeignFieldFunks {


  val dataSetName: String = dataSetSpec.name;
  val fldGenList: FieldGenList = new FieldGenList();
  //var fieldFunks: Map[String, List[() => GeneratedValue]] = Map.empty;
  //var dataSet: IDataSet = createDataSet(dataSetSpec, List());
  val state = Ref(BuilderState(-1L, None, None, Map.empty));
  val maxPartionSize: Long = 3000;
  //var theQ: ObjectGeneratorQueue = null;
  var dataSetSpec: IDataSetSpec = dss;
  var nRows: Int = 0;


  // var rvrMap: Map[String, RowValueRequest] = Map.empty;
  var requestDelegate: RequestDelegate = _

  //override var requestDelegate: RequestDelegate = new TableRequestDelegate(this);
  var objectCount: Long = 1L;
  private var done: Boolean = false;

  override def isDone: Boolean = this.done;

  def apply(): GeneratedValue = null

  def act() {

    loop {
      react {
        case InitBuilder(nRows: Long, treeRequest: TreeRequest) =>
          log("TableBuilder(" + dataSetName + ") got InitBuilder(" + nRows + ")");
          val (rows: Long, objQ, fldFunks) = onInit(nRows, treeRequest, bc);

          atomic {


            implicit txn => // nested atomic
              state.transform((bs: BuilderState) => bs.withDataSet(createDataSet(dss, List.empty)).withRows(rows).withQueue(objQ).withFieldFunctions(fldFunks));

              null;
          }


        case StartDataSetBuild =>
          log("TableBuilder(" + dataSetName + ") got " +
            "StartDataSetBuild msg");
          startBuild;


        case Stop =>
          onExit();


        case req: ForeignObjectFunctionRequest =>
          onForeignObjectFunctionRequest(req, sender.asInstanceOf[Actor]);

        case req: ForeignObjectFunctionResponse =>

          onForeignObjectFunctionResponse(req.requestId, req.funks);


        case req: ObjectRequest =>
          onObjectRequest(req, req.client);


        case obj: IDataRow =>
          //val nuObj =  publishObject(obj)  ;
          if (state.single.get.machineState != Finished) {
            atomic {
              implicit txn =>
                state.set(state() withNewObject (obj));
                null;
            }
            if (state.single.get.dataSet.get.rows.size >= state.single.get.nRows) {
              log("TableBuilder(" + dataSetName + ") datasize:" + state.single.get.dataSet.get.rows.size + " exceeds or equals nRows:" + state.single.get.nRows + ".");
              this ! BuildingDone;
            }
            //if (state.single.get.)
            log("TableBuilder(" + dataSetName + ") got New Object(" + obj + ")");
          }

        case BuildingDone =>
          log("TableBuilder(" + dataSetName + ") got BuildingDone.");
          atomic {
            implicit txn =>
              state.transform((bs: BuilderState) => bs asFinished);
              null;
          }
          state.single.get.theQ.get ! Stop;
          val eds = EndDataSet(state.single.get.dataSet.get);
          done = true;
          agent ! eds;
          bc ! eds;


        case _ => ;
      }
    }
  }

  def onExit() = {
    log("TableBuilder(" + dataSetName + ").onExit()");
    //requestDelegate ! Stop;

    atomic {
      implicit txn =>
        state().theQ.get.isShuttingDown = true;
        null;
    }

    exit();
  }

  def onInit(n: Long,
             treq: TreeRequest,
             sender: Actor):
  (Long, ObjectGeneratorQueue, Map[String, ObjectFieldGenFunc]) = {

    log("TableBuilder(" + dataSetName + ").onInit()");
    val fieldFunks = generateNormalFieldGenFunks(treq, n, sender, dataSetSpec);
    log("TableBuilder(" + dataSetName + ").onInit() created fieldFunks:" + fieldFunks);

    val hasForeignFields = (dss.fieldNames exists ((fld: String) => dss.isForeignRef(fld)));
    log("TableBuilder(" + dataSetName + ").onInit() : hasForeignFields=" + hasForeignFields);

    val postProcess = {
      (o: IDataRow) =>
        log("TableBuilder(" + dataSetName + ").postProcess(" + o.id + ")");
        val nuObj = publishObject(o)
        this ! nuObj;
        nuObj;
    }
    var theQ = new ObjectGeneratorQueue(Some(postProcess));
    theQ.start();
    theQ ! AddFunks(fieldFunks);
    // theQ ! postProcess;
    log("TableBuilder(" + dataSetName + ").onInit() created ObjectGeneratorQueue:" + theQ);
    if (hasForeignFields) {
      atomic {
        implicit txn => // nested atomic
          state.transform((bs: BuilderState) => bs withQueue theQ withMachineState ForeignFieldFunctionRequestPending);
          null;
      }
      sendForeignFunkRequest(sender)
    }
    else {
      atomic {
        implicit txn => // nested atomic
          state.transform((bs: BuilderState) => bs withFieldFunctions fieldFunks withQueue theQ withMachineState Initialized);
          null;
      }
    }

    (n, theQ, fieldFunks);
  }

  def transformToLocalObjectFields(req: ForeignObjectFunctionBCRequest, valueMap: Map[String, List[ValueFunction]]): Map[String, scala.List[ValueFunction]] = {
    val invertedMap = req.fieldsToGenerate map {
      (t: (String, String)) =>
        (t._2 -> t._1)
    } toMap;

    val newValueMap: Map[String, List[ValueFunction]] =
      invertedMap.keys.foldLeft(Map[String, List[ValueFunction]]()) {
        (acc: Map[String, List[ValueFunction]], fieldName: String) =>
          val x: Tuple2[String, List[ValueFunction]] = (invertedMap(fieldName) -> valueMap(fieldName))
          (x :: acc.toList).toMap
      }
    newValueMap;
  }

  def onForeignObjectFunctionResponse(reqId: String, foreignFunks: Map[String, List[ValueFunction]]) = {

    log("TableBuilder(" + dataSetName + ").onForeignObjectFunctionResponse() retrieved funks:" + foreignFunks);

    val currentState = state.single.get.machineState;
    if (currentState != ForeignFieldFunctionRequestPendingAndStartRevcd && currentState != ForeignFieldFunctionRequestPending)
      throw new
          BuilderException("Recieved unexpected ForeignObjectFunctionResponse: id:" + reqId + " funks:" + foreignFunks)


    val request = state.single.get.foreignRequestIds(reqId);

    val ff: Map[String, List[ValueFunction]] = transformToLocalObjectFields(request, foreignFunks);


    val objFldGenFunks: Map[String, ObjectFieldGenFunc] = valueFuncToObjectFieldGenFunc(ff, nRows)
    log("TableBuilder(" + dataSetName + ").onForeignObjectFunctionResponse() objFldGenFunks:" + objFldGenFunks);

    val allFunks = (state.single.get.fieldFunctions ::: objFldGenFunks).toMap;
    log("TableBuilder(" + dataSetName + ").onForeignObjectFunctionResponse() allFunks:" + allFunks);

    val nuMachineState = if (allFunks.size >= dss.fields.size) Initialized else currentState;
    log("TableBuilder(" + dataSetName + ").onForeignObjectFunctionResponse() nuMachineState:" + nuMachineState);
    atomic {
      implicit txn => // nested atomic
        state().theQ.get ! AddFunks(allFunks);
        state.transform((bs: BuilderState) => bs withMachineState nuMachineState withFieldFunctions allFunks withoutRequestId request.requestId);
        // if we are init'd then resend the Start msg - this time it will start
        if (state().machineState == Initialized || state().machineState == ForeignFieldFunctionRequestPendingAndStartRevcd)
          this ! StartDataSetBuild
        null;
    }


    log("TableBuilder(" + dataSetName + ").onForeignObjectFunctionResponse() created ObjectFieldGenFuncs:" + objFldGenFunks);

    //        // if we are init'd then resend the Start msg - this time it will start
    //        if(state.single.get.machineState == Initialized && state.single.get.machineState == ForeignFieldFunctionRequestPendingAndStartRevcd)
    //            this ! StartDataSetBuild

  }

  def onForeignObjectFunctionRequest(req: ForeignObjectFunctionRequest, bc: Actor) = {

    log("TableBuilder(" + dataSetName + ").onForeignObjectFunctionRequest(" + req + ")");
    // check to see if there are already enough withRows to handle the request.

    val retFuncMap = onForeignObjectFunctionRequestReal(req.requestId, req.nRows, req.fieldsToGenerate, req.client);
    //sender ! retFuncMap;
  }

  def onForeignObjectFunctionRequestReal(reqId: String,
                                         nRows: Long,
                                         fieldsToGenerate: Map[String, String], client: Actor): Map[String, List[ValueFunction]] = {
    log("TableBuilder(" + dataSetName + ").onForeignObjectFunctionRequestReal(" + nRows + "," + fieldsToGenerate + ")");

    atomic {
      implicit txn => // nested atomic

        state.transform((bs: BuilderState) => bs withRows (bs.nRows + nRows));

        null;
    }
    val x = sendRequestedData(reqId, nRows, fieldsToGenerate.values, client)
    log("TableBuilder(" + dataSetName + ").onForeignObjectFunctionRequestReal() sent funks(" + x + ") to " + client);
    x
  }

  def onObjectRequest(req: ObjectRequest, bc: Actor) = {
    log("TableBuilder(" + dataSetName + ").onForeignObjectFunctionRequest(" + req + ")");

    val retFuncMap: Map[String, List[ValueFunction]] =
      onForeignObjectFunctionRequestReal(req.requestId, req.nRows, req.fieldsToGen, req.client);
    val inverted: List[Map[String, ValueFunction]] = invertRetFuncMap(retFuncMap);

    val dataObjList = inverted map {
      (m: Map[String, ValueFunction]) =>
        val obj: IDataRow = createDataRow();
        m.foldRight(obj) {
          (t: (String, ValueFunction), acc: IDataRow) => acc + t;
        }
    }
    log("TableBuilder(" + dataSetName + ").onForeignObjectFunctionRequest(" + req + ") sending " + dataObjList + " to " + sender);
    req.client ! dataObjList;
  }

  def invertRetFuncMap(retFuncMap: Map[String, List[ValueFunction]]): List[Map[String, ValueFunction]] = {

    val n = retFuncMap.values.head.size;
    //            val mapList: List[Map[String, ValueFunction]] = 0 to n map
    //               {(i:Int)=>Map[String, ValueFunction]()};
    val mapList = List[Map[String, ValueFunction]]().padTo(n, Map[String, ValueFunction]());
    retFuncMap.keys.foldRight(mapList) {
      (fld: String, acc: List[Map[String, ValueFunction]]) =>
        val valueFuncList = retFuncMap.get(fld).get;
        val c: List[Map[String, ValueFunction]] = 0 to n map ((i: Int) => {
          acc(i) + (fld -> valueFuncList(i))
        }

          )
        c;
    }
  }

  @tailrec
  final def createDataRows(n: Long, r: List[IDataRow] = Nil): List[IDataRow] = {

    n match {
      case 0L => r;
      case _ => createDataRows(n - 1, createDataRow() :: r)
    }
  }

  def relationship(fieldName: String): Option[Relationship] = {
    this.dss.relationshipsFor(fieldName) match {
      case firstRel :: restOfRels => Some(firstRel)
      case Nil => None
    }

  }

  def disbursement(optRel: Option[Relationship], cnt: Long): Option[Disbursement] = {
    optRel match {
      case Some(r: Relationship) => disbursement(r, cnt);
      case _ => None;
    }
  }

  def disbursement(rel: Relationship, cnt: Long): Option[Disbursement] = {
    val flds = rel.fieldMap keys;
    //val reqId = createReqId(rel.dataSetName, this.iterableToList(flds));
    rel.defaultDisbursementSpec match {
      case Some(d: DisbursementSpec) => Some(new BasicDisbursement(d, cnt, 0, 0));
      case _ => None
    }
  }

  def createReqId(dsn: String, flds: List[String]): String = {
    flds mkString(
      dsn + ":" + "[",
      "+",
      "]" + (new java.util.Date()).getTime.toString);
  }

  /**
    * Builds the data set by the following steps:
    * 1) initialize the generator for each field
    * 2) generate normal fields ( not aggregates or foreign-refs)
    *
    *
    */
  def startBuild = {
    log("TableBuilder(" + dataSetName + ").startBuild() machineState:" + state.single.get.machineState);

    state.single.get.machineState match {
      case ForeignFieldFunctionRequestPending => {
        log("TableBuilder(" + dataSetName + ").startBuild() : still waiting for ForeignRows");

        atomic {
          implicit txn => // nested atomic
            state.transform((bs: BuilderState) => bs.withMachineState(ForeignFieldFunctionRequestPendingAndStartRevcd));

            null;
        }

      }
      case Initialized => {
        log("TableBuilder(" + dataSetName + ").startBuild()  recvd startBuild Message!")
        //val sds = StartDataSet(dataSetSpec);
        agent ! StartDataSet(dataSetSpec);


        state.single.get.theQ.get ! StartBuild
        /// add as many as needed to the queue
        for (i <- 0 until nRows) {
          val syncVar: SyncVar[IDataRow] = new SyncVar[IDataRow];
          val o = createDataRow();
          val f = {
            (x: IDataRow) => {
              log("TableBuilder(" + dataSetName + ").ObjGen.postProcess() publishing:" + x)

              val y = (x)
              log("TableBuilder(" + dataSetName + ").ObjGen.postProcess() setting syncvar to:" + y)

              syncVar.set(y);
              y;
            }
          };
          atomic {
            implicit txn =>
              // state().q.put(og) ;
              state().theQ.get ! ObjectGeneration(o, dataSetSpec.fieldNames, Map[String, GeneratedValue](), syncVar, Some(f));
              null;

          }


          log("TableBuilder(" + dataSetName + ") added the ObjectGeneration to theQ:" + state.single.get.theQ.get)
          //log("added the ObjectGeneration to theQ!")


        };

      } //case
      case Created =>
        log("TableBuilder Got Start Msg in " + Created.getClass.getName + " state.")


      case a: AnyRef => log("TableBuilder in bad State:" + a)
    } //match
  }

  def getDataSet(): IDataSet = state.single.get.dataSet.get

  /**
    * This method publishes this row to all that need to know
    * Some only need to know if the row is complete
    * This may vary from schemaType to schemaType (rdbms, noSQL)
    *
    */
  def publishObject(dataRow: IDataRow) = {
    log("TableBuilder(" + dataSetName + ") publishObject(" + dataRow + ")");

    //requestDelegate ! dataRow;
    if (dataRow.complete && !dataRow.published) {
      val gr = new GeneratedRow(dataSetSpec.name, dataRow)

      agent ! gr;
      bc ! gr;
      log("TableBuilder(" + dataSetName + ") publishObject published(" + dataRow + ")");
      dataRow.asPublished();
    }
    else {
      log("TableBuilder(" + dataSetName + ") publishObject - Incomplete or published(" + dataRow + ")");
      dataRow;
    }
  }

  override
  def toString(): String = "TableBuilder(" + dataSetName + ")";

  def createDataRow(): IDataRow = DataRow(this.dataSetSpec, createDataRowId(dss), false);

  def createDataSet(dss: IDataSetSpec, rowList: List[IDataRow]): IDataSet = DataSet(dss, rowList);

  def createDataRowId(dss: IDataSetSpec) = {
    objectCount += 1L;
    "Obj:" + dss.name + "." + (objectCount) + "." + (Calendar.getInstance().getTimeInMillis)
  }
}


case class ObjectGeneration(rowSoFar: IDataRow,
                            fields2Gen: List[String],
                            preDefValues: Map[String, GeneratedValue],
                            syncVar: SyncVar[IDataRow],
                            postProcess: Option[(IDataRow) => IDataRow]
                           );
