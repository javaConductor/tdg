package com.objectdynamics.tdg.builder.delegate

import com.objectdynamics.tdg.asyncmessages.RowValueRequest
import com.objectdynamics.tdg.builder.model.IDataRow

/**
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: Dec 9, 2010
 * Time: 12:47:51 AM
 * To change this template use File | Settings | File Templates.
 */
class UniqueDistribution(val rvr: RowValueRequest, var dataList: List[IDataRow]) extends Distribution
{

    def this(rvr: RowValueRequest) =
    {
        this (rvr, List())
    }

    override val dtype: String = "unique";

    def complete: Boolean = rvr.nRows <= dataList.size

    def process(partialRow: IDataRow): Distribution =
    {
        new UniqueDistribution(rvr, partialRow :: dataList)
    }

    def needsRow(pRow: IDataRow): Boolean = !(rvr.fieldNames forall
      {
          fnm: String =>
              !pRow.hasFields(Set[String](fnm))
      })
}

//class MySet()