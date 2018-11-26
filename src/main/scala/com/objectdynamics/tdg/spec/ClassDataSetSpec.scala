package com.objectdynamics.tdg.spec

import java.lang.reflect.Field

import com.objectdynamics.tdg.builder.model.IDataField
import com.objectdynamics.tdg.spec.datatypes.{DataType, Default}

import scala.reflect.runtime.universe._

/**
  * Created by lee on 8/10/17.
  */
object ClassDataSetSpec {
  val validTypes = List(classOf[Int].getName,
    classOf[Double].getName,
    classOf[String].getName,
    classOf[Float].getName,
    classOf[BigDecimal].getName,
    classOf[Short].getName)

  def defaultForType[T](implicit tag: WeakTypeTag[T], defVal: Default[T]): T = {
    defVal.default
  }

  //DataType.simpleTypes

  def apply[T](clazz: Class[T]): ClassDataSetSpec[T] = {
    val fields = clazz.getFields.toSet.filter(validField)
    // set the name of ds
    val dsName = clazz.getName

    // get the seq of DataFields from the fields
    val dataFields: Set[IDataField] = fields.map(fieldToDataField).filter(!_.isEmpty).map(_.get)
    new ClassDataSetSpec(clazz, dsName, dataFields)
  }

  def fieldToDataField(field: Field): Option[IDataField] = {
    classToDataType(field.getDeclaringClass) match {
      case Some(dt: DataType) => Some(new DataField(field.getName, dt))
      case _ => None
    }
  }

  def classToDataType[T](clazz: Class[T]): Option[DataType] = {
    DataType.fromString(clazz.getName)
  }

  def validField(field: Field): Boolean = {
    !field.isEnumConstant &&
      validTypes.contains(field.getDeclaringClass.getName)
  }
}

class ClassDataSetSpec[T](targetClass: Class[T],
                          name: String,
                          fields: Set[IDataField]) extends DataSetSpec(name, fields.toList)
