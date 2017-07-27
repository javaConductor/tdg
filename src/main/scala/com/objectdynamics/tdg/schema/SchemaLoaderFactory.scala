package com.objectdynamics.tdg.schema

object SchemaLoaderFactory {

  def createSchemaLoader(schemaType: String, argsMap: Map[String, String]): Option[MetadataLoader] = {

    schemaType match {
      case "json" => Some((new NativeJsonSchemaLoader));


      //not yet
      case "javaclass" => None;
      case "jdbc" => {

        None
      };

    }

  }

}
