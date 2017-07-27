package com.objectdynamics.tdg.builder.model

import com.objectdynamics.tdg.generators._

/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/9/11
 * Time: 12:42 AM
 */

trait IDataRow {

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

  def +(fldValue: NamedGeneratedValue[_]): IDataRow

  def +(dataItem: (String, GeneratedValue[_])): IDataRow

  def withDataRenamedTo(newNameMap: Map[String, String]): IDataRow

  def withFields(fldNames: Set[String]): IDataRow

  def hasFields(fldNames: Set[String]): Boolean

  def id: String
}