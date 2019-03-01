package com.objectdynamics.tdg.builder.model

import com.objectdynamics.tdg.generators._
import com.objectdynamics.tdg.util.{ListHelper, LogContributor}

/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/9/11
 * Time: 12:42 AM
 */
trait DataRow {

  val data: Map[String, GeneratedValue]

  def dss: IDataSetSpec

  //def values[U]: Map[String, GeneratedValue[U,TypeDef]]

  def value(fldName: String): Option[GeneratedValue] = {
    val f: String = fldName
    data(f) match {
      case (generatedValue: GeneratedValue) => Some(generatedValue)
      case _ => None
    }
  }

  def complete: Boolean

  def +(fldValue: GeneratedValue): DataRow

  def withDataRenamedTo(newNameMap: Map[String, String]): DataRow

  def withFields(fldNames: Set[String]): DataRow

  def hasFields(fldNames: Set[String]): Boolean

  def id: String
}

case class DefaultDataRow(dataSetSpec: IDataSetSpec,
                          objectId: String,
                          data: Map[String, GeneratedValue] = Map.empty)
  extends DataRow with LogContributor {

  var isComplete: Boolean = {
    val b = hasFields(ListHelper.listToSet(fields))
    // log(this + ": complete=" + b);
    b
  }

  def this(dataSetSpec: IDataSetSpec, objectId: String) = this(dataSetSpec, objectId, Map.empty)

  override def +(fldValue: GeneratedValue): DataRow = {
    this.copy(dataSetSpec = dss, data = this.data + (fldValue.name -> fldValue))
  }

  def complete: Boolean = isComplete

  def definesFields(flds: Set[String]): Boolean = flds.size == (fields intersect flds.toList).length;

  //var data:Map[String, Option[Any] ]
  def fields(): List[String] = dss.fields map {
    _.name
  }

  def dss: IDataSetSpec = {
    dataSetSpec
  }

  def withFields(flds: Set[String]): DefaultDataRow = {
    this.copy(
      dataSetSpec = this.dataSetSpec,
      data = fieldSet(flds)
    )
    DefaultDataRow(dataSetSpec, this.objectId, fieldSet(flds))
  }

  def fieldSet(flds: Set[String]): Map[String, GeneratedValue] = {
    hasFields(flds) match {
      case true => (flds map { fldname: String => (fldname -> value(fldname).get) }) toMap
      case false => Map.empty
    }
  }

  def hasFields(flds: Set[String]): Boolean = {
    flds.forall {
      data.get(_) match {
        case Some(x) => true;
        case None => false;
      }
    }
  }

  //val dataField:DataField;
  override def value(field: String): Option[GeneratedValue] = data.get(field)

  def withDataRenamedTo(fmap: Map[String, String]): DataRow = {
    val fields = fieldSet(fmap.keySet)
    this.copy(data = fields.map {
      (tpl) => {
        val name = tpl._1
        val gval = tpl._2
        fmap.get(name).get -> gval
      }
    }.toMap)
  }

  override def toString: String = "DataRow(" + dss.name + ")" + (".") + id

  def id: String = objectId
}
