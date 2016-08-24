package com.objectdynamics.tdg.builder.model

/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/9/11
 * Time: 1:04 AM
 */
;

trait IDataSet
{
    //def this(dss:B, withRows:List[Z]){}
    def row(idx: Int): Option[IDataRow];

    def rows: List[IDataRow];

    def +(r: IDataRow): IDataSet;

    def +(r: List[IDataRow]): IDataSet;

    def name: String;

    def dataObjectSpec: IDataSetSpec;

}