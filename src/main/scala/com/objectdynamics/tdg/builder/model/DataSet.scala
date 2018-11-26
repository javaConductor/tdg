package com.objectdynamics.tdg.builder.model

import com.objectdynamics.tdg.util.LogContributor

/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/9/11
 * Time: 1:04 AM
 */
trait DataSet[R >: DataRow, S] {
  def row(idx: Int): Option[R]

  def rows: List[R]

  def +(r: R): DataSet[R, S]

  def +(r: List[R]): DataSet[R, S]

  def name: String

  def dataObjectSpec: IDataSetSpec

}

trait IDefaultDataSet extends DataSet[DataRow, IDataSetSpec]


object DefaultDataSet {

  def apply(dataObjSpec: IDataSetSpec,
            dataRows: List[DataRow]): DefaultDataSet = new DefaultDataSet(dataObjSpec, dataRows)
}

/**
  * This class represents a Table in a RDBS or Collection in NO-SQL Data Store.
  */
class DefaultDataSet(val dataObjSpec: IDataSetSpec,
                     val dataRows: List[DataRow])
  extends IDefaultDataSet with LogContributor {

  override def name = dataObjectSpec.name

  override def dataObjectSpec = dataObjSpec

  def +(dr: DataRow): DefaultDataSet = DefaultDataSet(dataObjectSpec, dr :: rows)

  def +(r: List[DataRow]): IDefaultDataSet = DefaultDataSet(dataObjectSpec, r ::: rows)

  def rows: List[DataRow] = dataRows

  def row(i: Int): Option[DataRow] = {

    if (i < rows.size) {
      //val d = withRows(i);
      Some(rows.apply(i))
    }
    else {
      None
    }
  }

  override def toString: String = {
    val s: String = "DataSet(" + dataObjectSpec.name + ": " + rows.size + " withRows -->\n"
    /// print the headers
    val fldNames: List[String] = dataObjectSpec.fields.map {
      df: IDataField => df.name
    }

    s"""$s ${fldNames.mkString(",")}
       |
     """.stripMargin
  }
}
