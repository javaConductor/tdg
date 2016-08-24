/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.objectdynamics.tdg.asyncmessages

import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.parser.model._
import scala.concurrent.SyncVar
import com.objectdynamics.tdg.builder.delegate._
import com.objectdynamics.tdg.delivery.DeliveryAgent
import com.objectdynamics.tdg.builder._
import akka.actor._

trait Message extends IMessage
{

}

sealed trait NativeMessage extends Message;

/*
 *
 ,
 *
 */

/**
 * BuildCoordinator message
 */

case object StartBuild extends NativeMessage;

case class NewBuilder(b: IDataSetBuilder) extends NativeMessage;

case class InitBuildCoordinator(buildRequest: BuildRequest, agent: DeliveryAgent) extends NativeMessage;

case class InitBuilder(nRows: Long, treeRequest: TreeRequest) extends NativeMessage;

/**
 * The Final message sent in the test data process.  The entire data payload is usually delivered with this message.
 *
 */
case class TestDataComplete() extends NativeMessage;

//case class GenerateRowsNormalFieldsOnly(nRows: Long) extends NativeMessage;

// builderMgr -> Builder
//case object SendRequests extends NativeMessage;

// builderMgr -> Builder
case object StartDataSetBuild extends NativeMessage;

// builderMgr -> Builder
case class SendRequest(rvr: RowValueRequest) extends NativeMessage;


case class RequestedData(dataSet: String,
                         requestId: String,
                         fieldNames: Set[String],
                         data: List[IDataRow]) extends NativeMessage;


case class InitDelegate(foreignSyncMap: Map[String, SyncVar[List[IDataRow]]]) extends NativeMessage;

case class GeneratedRow(dataSet: String, dataRow: IDataRow) extends NativeMessage;

case class StartDataSet(dataSetSpec: IDataSetSpec) extends NativeMessage;

case class EndDataSet(dataSet: IDataSet) extends NativeMessage;

case class DataSetsCompleted() extends NativeMessage;

case class PostCompleteData(dataSetName: String, data: List[IDataRow]) extends NativeMessage;

case object Stop extends NativeMessage;
case object BuildingDone extends NativeMessage;
case class PartialRowRequest(id: Long,
                             fieldConstraintsMap: Map[String, FieldGenConstraints],
                             fields: List[IDataField],
                             nRows: Long) extends NativeMessage;

case class ForeignObjectFunctionRequest(requestId: String,
                                        client:Actor,
                                        treeRequest: TreeRequest,
                                        fieldsToGenerate:Map[String,String],
                                        preDefValues: Map[String, ValueFunction],
                                        nRows: Long) extends NativeMessage
{
    def this( tr:TreeRequest, bcReq:ForeignObjectFunctionBCRequest)  = {
        this(bcReq.requestId, bcReq.client, tr,bcReq.fieldsToGenerate, bcReq.preDefValues, bcReq.nRows)
    }
};
case class ForeignObjectFunctionBCRequest(requestId: String,
                                        client:Actor,
                                        dataSetName: String,
                                        fieldsToGenerate: Map[String,String],
                                        preDefValues: Map[String, ValueFunction],
                                        nRows: Long) extends NativeMessage;

case class ObjectRequest(requestId: String,
                                        reg: TreeRequest,
                                        client:Actor,
                         fieldsToGen:Map[String,String],
                         preDefValues: Map[String, ValueFunction],
                         nRows: Long) extends NativeMessage;


case class ForeignObjectFunctionResponse(requestId: String,
                                        funks:Map[String, List[ValueFunction]]) extends NativeMessage;
// builderMgr -> Builder
@deprecated
case class RowValueRequest(requestingField: String, dataSet: String,
                           delegate: RequestDelegate,
                           relIdx: Int,
                           requestId: String,
                           nRows: Long,
                           fieldNames: Set[String],
                           where: RequestConditions, rowOffset: Long, disbursementSize: Long,
                           unique: Boolean) extends NativeMessage;


 case class AddFunks(ff: Map[String, ObjectFieldGenFunc]) extends NativeMessage;
