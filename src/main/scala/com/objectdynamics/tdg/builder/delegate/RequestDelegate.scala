package com.objectdynamics.tdg.builder.delegate

import akka.actor._
import com.objectdynamics.tdg.asyncmessages._
import com.objectdynamics.tdg.builder.model._

import scala.concurrent.SyncVar

/**
  * Created by IntelliJ IDEA.
  * User: Lee
  * Date: Nov 21, 2010
  * Time: 3:32:04 AM
  * To change this template use File | Settings | File Templates.
  */

abstract class RequestDelegate(val builder: IDataSetBuilder) extends Actor {
  def receive = {

    case InitDelegate(foreignSyncMap: Map[String, SyncVar[List[IDataRow]]]) => onInitDelegate(foreignSyncMap);
    case rowReq: RowValueRequest => onRequest(rowReq);
    case reqData: RequestedData => onDataReceived(reqData);
    case dataRow: IDataRow => if (dataRow.complete) onDataRow(dataRow.dss.name, dataRow) else onPartialRow(dataRow.dss.name, dataRow);
    case Stop => onStop();
  }

  def onInitDelegate(foreignSyncMap: Map[String, SyncVar[List[IDataRow]]]);

  def onStop();

  def onRequest(rowReq: RowValueRequest);

  def onPartialRow(dataSet: String, pRow: IDataRow);

  def onDataRow(dataSet: String, row: IDataRow);

  def onDataReceived(reqData: RequestedData);
}

