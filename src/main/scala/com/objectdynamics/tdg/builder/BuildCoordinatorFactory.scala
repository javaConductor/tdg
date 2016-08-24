package com.objectdynamics.tdg.builder

import com.objectdynamics.tdg.delivery.DeliveryAgent
import sun.management.resources.agent

//import collection.mutable.HashMap

import com.objectdynamics.tdg.builder.model._

object BuildCoordinatorFactory
{

    def createBuildCoordinator( f:(ITestData => Unit),
                                outputType: String,
                               schemaType: String,
                               schema: ITestDataSchema,
                               agent: DeliveryAgent,
                               request: BuildRequest
                                ): Option[IBuildCoordinator] =
    {

        (outputType, schemaType) match
        {

            case ("csv", "json") =>
            {
                Some(new BC2(schema.asInstanceOf[ITestDataSchema], agent, request,f));
            }

            case _ => None;

        } //match
    } //def
}