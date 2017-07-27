package com.objectdynamics.tdg.builder.delegate

import com.objectdynamics.tdg.asyncmessages.{RequestedData, RowValueRequest}
import com.objectdynamics.tdg.builder.model._

/**
  * Created by IntelliJ IDEA.
  * User: Lee
  * Date: Dec 8, 2010
  * Time: 10:07:56 PM
  * To change this template use File | Settings | File Templates.
  */
class SharedDistribution(var sdd: SharedDistData) extends Distribution {
  override val dtype: String = "shared";

  def complete: Boolean = {
    sdd.data isEmpty
  }

  def add(rvr: RowValueRequest): Distribution = {
    var d2 =
      new SharedDistribution(SharedDistData(sdd.data));
    var bDone = false;
    var rfsdList: List[RvrFieldSetData] =
      sdd.data.map {
        rfsd: RvrFieldSetData =>
          if (rvr.fieldNames.equals(rfsd.fields)) {
            bDone = true;
            RvrFieldSetData(rvr.fieldNames, RvrData(rvr, List[IDataRow]()) :: rfsd.rvrList)
          }
          else {
            rfsd
          }
      }

    // this must be an entirely new fieldSet in this request
    if (!bDone) {
      // create a set with those fields and add it
      rfsdList = RvrFieldSetData(rvr.fieldNames, List(RvrData(rvr, List[IDataRow]()))) :: rfsdList
    }
    new SharedDistribution(SharedDistData(rfsdList))
  }

  def process(partialRow: IDataRow): Distribution = {
    val dtype: String = "shared";
    val newDist: SharedDistribution = new SharedDistribution(sdd);

    val (relevant, not_relevant) = sdd.data partition {
      rvrFldSetData: RvrFieldSetData =>
        partialRow.hasFields(rvrFldSetData.fields)
    }
    //removeEmpty(processRelevant(relevant));

    new SharedDistribution(
      SharedDistData(
        removeEmpty(processRelevant(partialRow, relevant)) ::: not_relevant
      )
    )
  }

  def removeEmpty(relevantList: List[RvrFieldSetData]): List[RvrFieldSetData] = {
    relevantList filter {
      (fsData: RvrFieldSetData) => !fsData.complete
    }
  }

  def processRelevant(pRow: IDataRow, relevantList: List[RvrFieldSetData]): List[RvrFieldSetData] = {
    val lst: List[RvrFieldSetData] = relevantList map {
      (fsData: RvrFieldSetData) =>
        /// Processing FieldSet 'fields'
        processFieldSet(pRow, fsData)
    }
    lst;
  }

  def processFieldSet(pRow: IDataRow, fsData: RvrFieldSetData): RvrFieldSetData = {
    var retRd: RvrFieldSetData = RvrFieldSetData(fsData.fields, fsData.rvrList.toList)
    val rvrDataList: List[RvrData] = fsData.rvrList map {
      applyValue(pRow, _)
    }
    val notFulfilledList = rvrDataList filter { rvrData: RvrData => !tryToFulfilRequest(rvrData) };
    RvrFieldSetData(fsData.fields, notFulfilledList);
  }

  def applyValue(pRow: IDataRow, rvrData: RvrData): RvrData = {
    RvrData(rvrData.rvr, (pRow withFields rvrData.rvr.fieldNames) :: rvrData.data);
  }

  def tryToFulfilRequest(rvrData: RvrData): Boolean = {
    if (rvrData.data.size >= rvrData.rvr.nRows) {
      fulfilRequest(rvrData);
      return true
    }
    return false
  }

  def fulfilRequest(rvrData: RvrData) {
    rvrData.rvr.delegate ! RequestedData(rvrData.rvr.dataSet, rvrData.rvr.requestId, rvrData.rvr.fieldNames, rvrData.data)
  }

  def needsRow(pRow: IDataRow): Boolean = {
    sdd.data.foldLeft(false)((acc, fsData) =>
      if (acc) {
        true
      }
      else {
        pRow.hasFields(fsData.fields)
      }
    )
  }

}
