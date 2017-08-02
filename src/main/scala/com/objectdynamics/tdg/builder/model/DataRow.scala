package com.objectdynamics.tdg.builder.model

import com.objectdynamics.tdg.generators._

/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/9/11
 * Time: 12:42 AM
 */
trait DataRow {

  def dss: IDataSetSpec

  def data: Map[String, GeneratedValue[_]]

  def value(fldName: String): GeneratedValue[_] = {
    val f: String = fldName
    data(f) match {
      case (generatedValue: GeneratedValue[_]) => generatedValue
      case _ => NullValue
    }
  }

  def complete: Boolean

  def +(fldValue: NamedGeneratedValue[_]): DataRow

  def +(dataItem: (String, GeneratedValue[_])): DataRow

  def withDataRenamedTo(newNameMap: Map[String, String]): DataRow

  def withFields(fldNames: Set[String]): DataRow

  def hasFields(fldNames: Set[String]): Boolean

  def id: String
}