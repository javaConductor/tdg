package com.objectdynamics.tdg.builder.delegate

import com.objectdynamics.tdg.asyncmessages.RowValueRequest
import com.objectdynamics.tdg.builder.model._

/**
  * Created by IntelliJ IDEA.
  * User: Lee
  * Date: Dec 8, 2010
  * Time: 10:28:32 PM
  * To change this template use File | Settings | File Templates.
  */
case class RvrFieldSetData(fields: Set[String], rvrList: List[RvrData]) {
  def this() = {
    this(Set[String](),
      List[RvrData]())
  }

  def this(rvr: RowValueRequest) = this(rvr.fieldNames, List(RvrData(rvr, List[IDataRow]())))

  def complete: Boolean = {
    rvrList isEmpty
  }

  def purge(): RvrFieldSetData = {

    val (complete, incomplete) = rvrList partition { rvrData: RvrData => rvrData.complete }
    RvrFieldSetData(fields, incomplete);
  }

  //
  //    def +(rvr: RowValueRequest): RvrFieldSetData =
  //    {
  //        RvrFieldSetData(fields,   List[RvrData](RvrData(rvr, List())))
  //    }

}

//RvrData

case class RvrData(rvr: RowValueRequest, data: List[IDataRow]) {
  def complete = rvr.nRows <= data.size;
}

//SharedDistData

case class SharedDistData(data: List[RvrFieldSetData]) {
  def this() {
    this(List[RvrFieldSetData]());
  }


  def +(rvr: RowValueRequest): SharedDistData = {
    val rfsd: Option[RvrFieldSetData] = data.find { rr: RvrFieldSetData => rvr.fieldNames.equals(rr.fields) };
    rfsd match {
      case Some(r: RvrFieldSetData) => SharedDistData(r :: data);
      case None => {
        val rvrData: RvrData = RvrData(rvr, List[IDataRow]());
        val newRfsd = RvrFieldSetData(rvr.fieldNames, List(rvrData));
        SharedDistData(newRfsd :: data);
      }
    }
  }

}

//SharedDistData