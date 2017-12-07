package com.objectdynamics.tdg.spec

import java.lang.reflect.Field

import com.objectdynamics.tdg.builder.model.IDataField
import com.objectdynamics.tdg.spec.datatypes.{DataType, Default}

import scala.reflect.runtime.universe._

/**
  * Created by lee on 8/10/17.
  */

case class DataField(val name: String,val dataType: DataType) extends IDataField

object ClassDataSetSpec {
  def defaultForType[T](implicit tag: WeakTypeTag[T], defVal: Default[T]): T = {
    defVal.default
  }
  def fieldToDataField(field: Field) : IDataField  = {
    new DataField(field.getName, classToDataType(field.getDeclaringClass))
  }

DataType.simpleTypes

  val validTypes = List( classOf[Int].getName,
    classOf[Double].getName,
    classOf[String].getName,
    classOf[Float].getName,
    classOf[BigDecimal].getName,
    classOf[Short].getName )

  def validField(field: Field): Boolean = {
    !field.isEnumConstant &&
    validTypes.contains(field.getDeclaringClass.getName)
  }

  def classToDataType[T](clazz: Class[T]) : DataType = {
    DataType.fromString(clazz.getName, 0)
  }

  def apply[T](clazz: Class[T]): ClassDataSetSpec[T] = {
    val fields = clazz.getFields.toSet.filter(validField)
    // set the name of ds
    val dsName = clazz.getName

    // get the seq of DataFields from the fields
    val dataFields: Set[IDataField] = fields.map(fieldToDataField)
    new ClassDataSetSpec(clazz, dsName, dataFields)
  }
}

class ClassDataSetSpec[T](targetClass: Class[T],
                          name:String,
                          fields: Set[IDataField]) extends DataSetSpec(name, fields.toList)
