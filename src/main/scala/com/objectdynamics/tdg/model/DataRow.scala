package com.objectdynamics.tdg.model

import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.generators.{GeneratedValue, NamedGeneratedValue}
import com.objectdynamics.tdg.util._

/**
  * This class represents a row in RDBMS Table or Key/Value pair in a No-SQL Collection
  *
  * C
  * D <: IDataTypeObject,
  * E <: IDataTypeInstance,
  * B <: IDataSetSpec[IDataField[D,E]]
  *
  */
case class DataRow(dataSetSpec: IDataSetSpec,
                   objectId: String,
                   data: Map[String, GeneratedValue[_]] = Map.empty)
  extends IDataRow with LogContributor {

  var isComplete: Boolean = {
    val b = hasFields(ListHelper.listToSet(fields))
    // log(this + ": complete=" + b);
    b

  }

  def this(dataSetSpec: IDataSetSpec, objectId: String) = this(dataSetSpec, objectId, false);

  override def +(fldValue: NamedGeneratedValue[_]): IDataRow = this + (fldValue.name, fldValue)

  override def +(dataItem: (String, GeneratedValue[_])): IDataRow = {
    this.copy(dataSetSpec = dss, data = this.data + (dataItem._1 -> dataItem._2))
  }


  def complete: Boolean = isComplete

  def hasFields(flds: Set[String]): Boolean = {
    flds.forall {
      data.get(_) match {
        case Some(x) => true;
        case None => false;
      }
    }
  }

  def definesFields(flds: Set[String]): Boolean = flds.size == (fields intersect flds.toList).length;

  //var data:Map[String, Option[Any] ]
  def fields(): List[String] = dss.fields map {
    _.name
  };

  //val dataField:DataField;
  override def value(field: String): Option[GeneratedValue[_]] = data.get(field)

  def withFields(flds: Set[String]): DataRow = this.copy(dataSetSpec = dss, data = fieldSet(flds));

  def dss: IDataSetSpec = {
    dataSetSpec
  }

  def fieldSet(flds: Set[String]): Map[String, GeneratedValue[_]] = {
    hasFields(flds) match {
      case true => (flds map { fldname: String => (fldname -> value(fldname).get) }) toMap
      case false => Map.empty
    }
  }

  def withDataRenamedTo(fmap: Map[String, String]): IDataRow = {
    val fields = fieldSet(fmap.keySet)
    this.copy(data = fields.map {
      (tpl) => {
        val name = tpl._1
        val gval = tpl._2
        fmap.get(name).get -> gval
      }
    })
  }

  override def toString: String = "DataRow(" + dss.name + ")" + (".") + id

  def id: String = objectId;
}

