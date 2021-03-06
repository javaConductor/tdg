/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/13/11
 * Time: 3:31 AM
 */
package com.objectdynamics.tdg.spec.datatypes

import com.objectdynamics.tdg.generators.GeneratedValue

trait IDataTypeInstance
{
    val name: String;

    def fromString(s: String): Option[_];
    type scalaClass;

    //def toString(datum: Option[_]): String;
    def toString(datum: GeneratedValue): String;

    def objectFromString(strRep: String): Option[scalaClass];
    //def generatedValueFromString(strRep: String): GeneratedValue
}