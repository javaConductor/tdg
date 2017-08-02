package com.objectdynamics.tdg.model

//import com.objectdynamics.tdg.objectgraph.datatypes.DataType

import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.util.LogContributor

/**
  * This class represents a Table in a RDBS or Collection in NO-SQL Data Store.
  */
case class DefaultDataSet(dataObjSpec: IDataSetSpec,
                          dataRows: List[DataRow])
  extends DataSet with LogContributor {

  override def name = dataObjectSpec.name

  override def dataObjectSpec = dataObjSpec


  //def dataRows(): List = withRows.toList

  def add(dataRow: DataRow) = this + dataRow;

  def +(dr: DataRow): DataSet = new DefaultDataSet(dataObjectSpec, dr :: rows)

  def rows: List[DataRow] = dataRows;

  def +(r: List[DataRow]): DataSet = new DefaultDataSet(dataObjectSpec, r ::: rows);

  def row(i: Int): Option[DataRow] = {

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
