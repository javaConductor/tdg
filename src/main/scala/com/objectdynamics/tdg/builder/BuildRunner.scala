/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.objectdynamics.tdg.builder

import ca.zmatrix.utils.ParseParms
import com.objectdynamics.tdg.delivery._
import java.io.File
import com.objectdynamics.tdg.asyncmessages._
import com.objectdynamics.tdg.util._
import com.objectdynamics.tdg.model.TestData
import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.schema._
import sun.management.resources.agent
import scala.concurrent.SyncVar
import akka.actor._

class BuildRunner(args: Array[String]) extends LogContributor
{
      val svTestData:SyncVar[ITestData] = new SyncVar[ITestData];
    def testData: ITestData = svTestData.get;

    log("BuildRunner starting... - " + args.toList.toString);
    go();

    def go() =
    {
        val tdgPath: String = determineTdgPathIfAny match
        {
            case Some(p) => p;
            case _ =>
            {
                ""

            }
        };
        val helpString = " -schema schemaFile  -count 26000 -outputType csv -outputFolder ./data "
        val pp = new ParseParms(helpString)
        pp.parm("-schema").rex("^.*\\.json$").req(true) // required
        .parm("-ds", "^\\*$", true) // alternate form, required
        .parm("-schemaType", "^\\*$", true) // the type of schema: json,ddl,javaClasses
        .parm("-count", "^\\d{7}$", true) // alternate form, required
        .parm("-outputType", "csv", true) //.rex("^\\[csv|json|jdbc]$")
        .parm("-tdgPath", tdgPath, false)
        .parm("-script" )
        .parm("-outputFolder", "./data", false)
        .parm("-dbInfo", "realStuff").req(false)

        // optional
        val result = pp.validate(args.toList)
        if(result._1)
        {
            val argsMap = result._3;
            val schema: ITestDataSchema = createSchema(argsMap) match
            {
                case Some(ts) => ts;
                case _ => log("BuildRunner: Could not create TestData Schema of type:" + argsMap.get("-schemaType").get);
                throw new
                    BuilderException("BuildRunner: Could not create TestData Schema of type:" + argsMap.get("-schemaType").get);
            }

            val agent = createAgent(argsMap) match
            {
                case Some(ag) => ag;
                case _ => log("BuildRunner: Could not create Delivery Agent:" + argsMap.get("-outputType").get);
                throw new
                    BuilderException("BuildRunner: Could not create Delivery Agent:" + argsMap.get("-outputType").get);
            }
            val breq: BuildRequest = createBuildRequest(argsMap);


            val f:(ITestData => Unit) = {(td:ITestData) =>

              svTestData.set(td);
                log("BuildRunner: Created TestData:" + td);
                log("BuildRunner: Exiting!" );


            }

            val bc: IBuildCoordinator = BuildCoordinatorFactory.createBuildCoordinator(   f,
                argsMap.get("-outputType").get,
                argsMap.get("-schemaType").get,
                schema, agent, breq) match
            {
                case Some(x: IBuildCoordinator) => x;
                case _ => log("BuildRunner: Could not coordinate build.");
                throw new BuilderException("BuildRunner: Could not create buildCoordinator.")
            };

            argsMap.get("script") match {

                case Some(s:String) =>
                    scriptRun(schema, s, bc, agent);

                case None =>
                    normalRun(schema, breq, bc, agent);


            }


            //val nRun = createRunFunction(schema, breq, agent);


        }
        else
        {
            log("BuildRunner: Bad CommandLine: " + result._2);

        }
    }

    //    def createRunFunction(schema: ITestDataSchema, breq: BuildRequest, agent: DeliveryAgent with Actor) = new
    //        normalRun(schema, breq, agent);

    def createAgent(argsMap: Map[String, String]) =
    {
        DeliveryAgentFactory.createDeliveryAgent(argsMap.get("-outputType") match
                                                 {
                                                     case Some(typ) => typ
                                                     case _ => "csv"
                                                 }, argsMap)
    }

    def createBuildRequest(argsMap: Map[String, String]): BuildRequest =
    {

        log("BuildRunner.createBuildRequest: argsMap:" + argsMap);
        val rootObj: String = argsMap.get("-ds") match
        {
            case Some(dsName) => log("createBuildRequest: -ds:" + dsName); dsName
            case _ => log("createBuildRequest: -ds not set:" + argsMap.get("-ds")); "";
        }
        val nRows: Int = argsMap.get("-count") match
        {
            case Some(cnt: String) => Integer.parseInt(cnt)
            case _ => throw new BuilderException("'-count n' is required.")
        }
        new BuildRequest(rootObj, nRows)
    }

    def determineTdgPathIfAny: Option[String] =
    {

        val p = System.getenv("TDG_PATH");
        if(p == null)
        {
            val userHome = System.getProperty("user.home");
            val f: File = new File(userHome, ".tdg");
            if(!f.exists)
            {
                f.mkdir
                initTdgFolder(f);
            }
            Some(f.getAbsolutePath)
        }
        else
        {
            Some(p);
        };
    }

    def initTdgFolder(f: File) =
    {
        // create the folders

        var ff: File = new File(f, "dbConfig");
        ff.mkdir

        ff = new File(f, "data");
        ff.mkdir;
        ff = new File(ff, "predef");
        ff.mkdir

        ff = new File(f, "schema");
        ff.mkdir

    }

    type SchemaLoader = MetadataLoader;

    type TestDataSchema = ITestDataSchema;

    def createSchema(argsMap: Map[String, String]): Option[ITestDataSchema] =
    {
        val schemaFile: String = argsMap.get("-schema") match
        {
            case Some(s) => s
            case _ => throw new BuilderException("-schema required ");
        }
        val schemaType: String = argsMap.get("-schemaType") match
        {
            case Some(s) => s
            case _ => "json";
        }

        log("BuildRunner.createSchema() from file:" + schemaFile);

        val jsonSchema = FileHelper.fileAsString(new File(schemaFile));
        val schemaLoader: MetadataLoader = SchemaLoaderFactory.createSchemaLoader(schemaType, argsMap) match
        {
            case Some(x) => x;
            case _ => throw new BuilderException("Unsupported schemaType: " + schemaType);
        }

        Some(schemaLoader.fromFile(schemaFile));
    }

    def normalRun(schema: ITestDataSchema,
                  buildReq: BuildRequest,
                  buildCoordinator: IBuildCoordinator with Actor,
                  agent: DeliveryAgent with Actor) =
    {

        // create and start deliveryAgent
        //agent.start();

        // create and start build Coordinator
        //buildCoordinator.start();

        // send Start msg to builder
         buildCoordinator ! StartBuild

        // create a SyncVar to hold the TestData
        svTestData.get
        log("BuildRunner.normalRun() -> StartBuild to BC ")

    }


     def scriptRun(schema: ITestDataSchema,
                  script:String,
                  buildCoordinator: IBuildCoordinator with Actor,
                  agent: DeliveryAgent with Actor) =
    {

        // create and start deliveryAgent
        //agent.start();

        // create and start build Coordinator
       // buildCoordinator.start();

        // send Start msg to builder
         buildCoordinator. ! StartBuild

        // create a SyncVar to hold the TestData
        svTestData.get
        log("BuildRunner.scriptRun() -> StartBuild to BC ")

    }
}


