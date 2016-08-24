/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/13/11
 * Time: 3:31 AM
 */
package com.objectdynamics.tdg.spec.datatypes

;

trait IDataTypeObject
{
    def fromString(dtype: String, size: Int): IDataTypeInstance;

    def fromString(dtype: String): IDataTypeInstance = fromString(dtype, 0);

    def fromOption(dtype: Option[String]): IDataTypeInstance;

    def typeMap(): Map[String, IDataTypeInstance];


}