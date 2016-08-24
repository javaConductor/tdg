package com.objectdynamics.tdg.builder.model

import com.objectdynamics.tdg.generators._
import com.objectdynamics.tdg.builder.ValueFunction

/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/9/11
 * Time: 12:42 AM
 */

trait IDataRow
{

    def dss: IDataSetSpec;

    def data: Map[String, ValueFunction];

    def value(fldName: String): ValueFunction =
    {
        val f: String = fldName;
        data(f) match
        {
            case (gval: ValueFunction) => gval;
            case _ => nullObjectFunction
        }
    };

    def published: Boolean;

    //def published_=(b: Boolean): Unit;
    def asPublished(): IDataRow;

    def complete: Boolean;

    def +(fldValue: Tuple2[String, ValueFunction]): IDataRow;

    def +(fldValue: NamedGeneratedValue): IDataRow;

    def withDataRenamedTo(newNameMap: Map[String, String]): IDataRow;

    def withFields(fldNames: Set[String]): IDataRow;

    def hasFields(fldNames: Set[String]): Boolean;

    def id: String;
}