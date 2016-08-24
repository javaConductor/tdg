package com.objectdynamics.tdg.builder.model

import akka.actor.Actor

import akka.actor._
import com.objectdynamics.tdg.delivery.DeliveryAgent
import com.objectdynamics.tdg.builder._
import com.objectdynamics.tdg.model.TestData

/*
* Created by IntelliJ IDEA.
* User: Lee
* Date: 3/10/11
* Time: 1:16 AM
*/
trait IBuildCoordinator
  extends Actor
{

    //def builder(dataSetName: String): Option[IDataSetBuilder];


    /**
     * This method is called to begin the building of test data.
     *
     */
    def startBuild( ) ;

    def schema: ITestDataSchema;


    /**

     */
    def newBuilder(ds: IDataSetSpec, bc: IBuildCoordinator): IDataSetBuilder;
}

case class BuildCoordinatorState(builders: List[IDataSetBuilder],
                                 buildRequest: Option[BuildRequest],
                                 agent: Option[DeliveryAgent],
                                 completeDataSets: List[String], //data set names
                                 symbolTable: Option[SymbolTable],
                                 testData: Option[ITestData],
                                 f:Option[(ITestData => Unit)] = None ,
                                 finished: Boolean = false)
{

    //def this() = this (Nil, None, None, Nil, None,Some(TestData(Set.empty)),  None);
    def this(f: (ITestData => Unit) ) = this (Nil, None, None, Nil, None,Some(TestData(Set.empty)),  Some(f) );

    def builder(nm: String) = builders.find((b: IDataSetBuilder) => b.dataSetSpec.name.equals(nm));

    def withSymbolTable(st: SymbolTable) = this.copy(symbolTable = (if(st == null) None else Some(st)));

    def withBuildRequest(br: BuildRequest) = this.copy(buildRequest = (if(br == null) None else Some(br)));

    def withAgent(ag: DeliveryAgent) = this.copy(agent = (if(ag == null) None else Some(ag)));

    def withReturnDataFunction(funk: (ITestData => Unit)) = this.copy(f = Some(funk) );

    def withTestData(td: ITestData) = this.copy(testData = (if(td == null) None else Some(td)));

    def withCompletedDataSets(cds: List[String]) = this.copy(completeDataSets = (if(cds == null) Nil else cds));

    def withBuilders(b: List[IDataSetBuilder]) = this.copy(builders = (if(b == null) Nil else b));

    def dataSetsComplete: Boolean = completeDataSets.size >= builders.size;

    def asFinished = this.copy(finished = true);

};
