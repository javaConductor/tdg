package com.objectdynamics.tdg.delivery

object DeliveryAgentFactory
{

    def createDeliveryAgent(outputType: String, argsMap: Map[String, String]): Option[DeliveryAgent] =
    {

        outputType match
        {
            case "csv" => Some(new CsvDeliveryAgent(argsMap.get("-outputFolder") match
                                                    {
                                                        case Some(folder) => folder
                                                        case _ => "."
                                                    }));
            case _ => None;
        }
    }
}