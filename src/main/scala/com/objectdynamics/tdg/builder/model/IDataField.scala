package com.objectdynamics.tdg.builder.model

/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/9/11
 * Time: 12:45 AM
 */

import com.objectdynamics.tdg.spec.DisbursementSpec
import com.objectdynamics.tdg.spec.datatypes._

trait IDataField /*[+D <: IDataTypeObject, +E <: IDataTypeInstance]*/
{
    val name: String;
    val fieldType: FieldType;
    val dataType: IDataTypeInstance;

    def isAggregate: Boolean;

    def primaryKey: Boolean;

    def generatorName: Option[String] = None;

    def setGeneratorName(n: String): Unit;

    def data: Option[List[_]] = None;

    //    var foreignRef: Option[ForeignRef] = None;
    def disbursementSpec: Option[DisbursementSpec] = None;

    // var foreignAgg: Option[SelfRef];
    def maxLength: Option[Int] = None;

    def min: Option[BigDecimal] = None;

    def max: Option[BigDecimal] = None;

    def unique: Boolean = false;

    def prefix: Option[String] = None;

    def suffix: Option[String] = None;

}