/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/13/11
 * Time: 1:54 AM
 */
package com.objectdynamics.tdg.builder.model

;

sealed trait FieldType
{

    //TODO add to/from String methods

}

object ASingleValue extends FieldType;

object AnObjectArray extends FieldType;

object AValueArray extends FieldType;

object AnObject extends FieldType;

