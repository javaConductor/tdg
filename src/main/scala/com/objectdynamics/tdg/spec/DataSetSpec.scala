package com.objectdynamics.tdg.spec

import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.util._

object DataSetType {

}

trait DataSetType {

}

object RDBMS extends DataSetType

object DOCUMENT extends DataSetType

/**
  * This clas represents
  */
case class DataSetSpec(override val name: String,
                       fields: List[IDataField])
  extends IDataSetSpec {
  var fieldMap: Map[String, IDataField] = createFieldMap(fields);
  var dataSetType: DataSetType = com.objectdynamics.tdg.spec.RDBMS;

  def getField(fieldName: String): Option[IDataField] = field(fieldName)

  def field(fieldName: String): Option[IDataField] = fieldMap.get(fieldName)

  def normalFields: List[IDataField] = fields filter { dataFld: IDataField => isNormal(dataFld.name) }

  def isNormal(fldName: String): Boolean = true

  def +(df: IDataField): IDataSetSpec =
    new DataSetSpec(name, df :: fields);

  /**
    *
    * @param fields :List[DataField]
    * @return
    */
  def createFieldMap(fields: List[IDataField]): Map[String, IDataField] = {
    ListHelper.listToMap[String, IDataField](fields, { f => f.name })
  };

  def fieldNames: List[String] = (fieldMap keys) toList;
}

