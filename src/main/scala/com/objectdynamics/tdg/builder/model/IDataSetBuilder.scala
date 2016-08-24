package com.objectdynamics.tdg.builder.model

/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/9/11
 * Time: 1:12 AM
 */

import akka.actor.Actor
import com.objectdynamics.tdg.builder.delegate._

trait IDataSetBuilder extends Actor
{

    var requestDelegate: RequestDelegate;
    //var dataSet: IDataSet;
    var dataSetSpec: IDataSetSpec;

    def createDataRow(): IDataRow;

    def createDataSet(dss: IDataSetSpec, rowList: List[IDataRow]): IDataSet;

}
