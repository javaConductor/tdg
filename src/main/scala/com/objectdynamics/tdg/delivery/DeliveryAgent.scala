package com.objectdynamics.tdg.delivery

import akka.actor._
import com.objectdynamics.tdg.asyncmessages._
import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.util.LogContributor

/**
  * Created by IntelliJ IDEA.
  * User: Lee
  * Date: Nov 21, 2010
  * Time: 4:35:44 AM
  * To change this template use File | Settings | File Templates.
  */

abstract class DeliveryAgent() extends Actor with LogContributor {
  def recieve = {
    case gr: GeneratedRow =>
      log("DeliveryAgent->" + gr);
      onGeneratedRow(gr.dataRow);
    case eds: EndDataSet =>
      log("DeliveryAgent->" + eds);
      onEndDataSet(eds.dataSet);
    case sds: StartDataSet =>
      log("DeliveryAgent->" + sds);
      onStartDataSet(sds.dataSetSpec);
    case Stop =>
      log("DeliveryAgent->Stop");
      onStop();
    case x: Any => log("BAD MSG: DeliveryAgent->" + x.toString)
  }

  def onGeneratedRow(dataRow: IDataRow): Unit;

  def onStop(): Unit;

  def onEndDataSet(dateSet: IDataSet): Unit;

  def onStartDataSet(dataSetSpec: IDataSetSpec): Unit;
}
