package com.objectdynamics.tdg.builder

import com.objectdynamics.tdg.delivery.DeliveryAgent

//import collection.mutable.HashMap

import delegate.RequestDelegate
import com.objectdynamics.tdg.builder.model._
import akka.actor._
import com.objectdynamics.tdg.util._
;


/*
Waits on a StartBuild message
then ...
*/

abstract class AbstractBuildCoordinator
  extends IBuildCoordinator
  with LogContributor with ListUtil
{
    //type TRequestDelegate = RequestDelegate[A, B, C,  X, Y, Z]
    //Builder[TTestDataSchema, TDataSetSpec, TDataField, TTestData, TDataSet, TDataRow];

    val agent: DeliveryAgent;
    val logger = org.apache.log4j.Logger.getLogger(getClass);
    //var builders: Map[String, IDataSetBuilder];
    // = Map[String, R]();
    var builderDelegates: Map[String, RequestDelegate] = Map.empty;
    var buildStarter: scala.actors.OutputChannel[Any] = _;

    /**
     * This method is called to begin the building of test data.
     *
     */
    def startBuild();


    /**

     */
    def newBuilder(ds: IDataSetSpec, bc: IBuildCoordinator): IDataSetBuilder;

}
