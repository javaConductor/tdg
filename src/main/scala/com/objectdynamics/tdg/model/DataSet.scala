package com.objectdynamics.tdg.model

//import com.objectdynamics.tdg.objectgraph.datatypes.DataType

import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.util.LogContributor

/**
  * This class represents a Table in a RDBS or Collection in NO-SQL Data Store.
  */
case class DataSet(dataObjSpec: IDataSetSpec,
                   dataRows: List[IDataRow])
  extends IDataSet with LogContributor {

  override def name = dataObjectSpec.name

  override def dataObjectSpec = dataObjSpec


  //def dataRows(): List = withRows.toList

  def add(dataRow: IDataRow) = this + dataRow;

  def +(dr: IDataRow): IDataSet = new DataSet(dataObjectSpec, dr :: rows)

  def rows: List[IDataRow] = dataRows;

  def +(r: List[IDataRow]): IDataSet = new DataSet(dataObjectSpec, r ::: rows);

  def row(i: Int): Option[IDataRow] = {

    if (i < rows.size) {
      //val d = withRows(i);
      Some(rows.apply(i));
    }
    else {
      None
    }
  }

  override def toString: String = {

    var s: String = "DataSet(" + dataObjectSpec.name + ": " + rows.size + " withRows -->\n"
    /// print the headers
    val fldNames: List[String] = dataObjectSpec.fields.map {
      df: IDataField => df.name
    }

    s"""$s ${fldNames.mkString(",")}
       |
     """.stripMargin
  }

  //end row i
}
