package com.objectdynamics.tdg.delivery

import actors.Actor
import com.objectdynamics.tdg.util._
import java.io._
import java.text._
import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.builder.ValueFunction
import com.objectdynamics.tdg.generators._

/**
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 12/18/10
 * Time: 11:35 PM

 */

class CsvDeliveryAgent(rootFolder: String) extends DeliveryAgent with LogContributor
{
    var dataRowHandlerMap: Map[String, CsvDataRowHandler] = Map[String, CsvDataRowHandler]();

    def onStartDataSet(dataSetSpec: IDataSetSpec): Unit =
    {
        log("CsvDeliveryAgent.onStartDataSet:" + dataSetSpec)
        val drh = new CsvDataRowHandler(dataSetSpec, rootFolder);
        drh.start;
        dataRowHandlerMap += (dataSetSpec.name -> drh);
    }

    def onGeneratedRow(dataRow: IDataRow): Unit = sendRowToHandler(dataRow.dss.name,
                                                                   dataRow)

    def onStop(): Unit =
    {
        log("CsvDeliveryAgent.onStop() - Exiting");
//        dataRowHandlerMap.values foreach
//          {drh: CsvDataRowHandler => drh ! com.objectdynamics.tdg.asyncmessages.Stop}
        this.exit
    }

    def onEndDataSet(dataSet: IDataSet): Unit = sendToHandler(dataSet.name, NoMoreRows)

    def sendRowToHandler(dataSet: String, dr: IDataRow): Unit =
    {
        sendToHandler(dataSet, dr);
    }

    def sendToHandler[A](dataSet: String, msg: A): Unit =
    {
        log("CsvDeliveryAgent.sendToHandler(" + dataSet + "," + msg + ")");

        dataRowHandlerMap.get(dataSet) match
        {
            case Some(csvDataRowHandler: Actor) =>
                log("CsvDeliveryAgent.sendToHandler(): sending " + msg + " to " + csvDataRowHandler);
                csvDataRowHandler ! msg;

            case None =>
            {
                log("CsvDeliveryAgent.sendToHandler(): NO message sent to  csvDataRowHandler");
            }
        };
    }
}

case class NoMoreRows();

class CsvDataRowHandler(dataSetSpec: IDataSetSpec, rootFolder: String) extends Actor with NoLogContributor
{
    // Create the file and write the header row
    val fw = new FileWriter(new File(rootFolder, dataSetSpec.name + ".csv").getAbsolutePath);

    def act()
    {
        initFile(fw, dataSetSpec);
        loop
        {

            react
            {
                case dr: IDataRow =>
                    log("CsvDataRowHandler(" + dataSetSpec.name + ") -> " + dr);
                    writeRow(fw, dr)
                case eds: NoMoreRows =>
                    log("CsvDataRowHandler(" + dataSetSpec.name + ") -> NoMoreRows");
                    onEndDataSet();

            }
        }

    }

    def initFile(fwriter: FileWriter, dataSetSpec: IDataSetSpec)
    {
        log("CsvDataRowHandler(" + dataSetSpec.name + ").initFile(" + dataSetSpec.name + ")");
        /// write the headers
        var bFirst = true;
        dataSetSpec.fields foreach
          {
              df: IDataField =>
                  if(!bFirst)
                  {
                      fwriter.write(",")
                  };
                  bFirst = false;
                  fwriter.write(df.name)
          }
        fwriter.write("\n")
        fwriter.flush()
    }

    def onEndDataSet() =
    {
        log("CsvDataRowHandler(" + dataSetSpec.name + ").onEndDataSet() - Exiting");
        /// close file and ...
        fw.close
        this.exit
    };

    def writeRow(fwriter: FileWriter, dataRow: IDataRow)
    {
        //log("CsvDataRowHandler(" + dataSetSpec.name + ").writeRow(" + dataRow + ")");
        writeColumns(fwriter, dataRow, dataRow.dss.fields);
    }

    private def writeColumns(fwriter: FileWriter, dataRow: IDataRow, dataFlds: List[IDataField]): Unit =
    {
        dataFlds match
        {
            case Nil => fwriter.write("\n");
            case dataFld :: restOfFlds =>
                writeColumn(fwriter, dataRow, dataFld);
                if(restOfFlds != Nil)
                {
                    fwriter.write(",")
                }
                writeColumns(fwriter, dataRow, restOfFlds);
        }
    }

    val dtFormat: DateFormat = new SimpleDateFormat("MM/dd/yyyy")

    def writeColumn(fwriter: FileWriter, dataRow: IDataRow, dataFld: IDataField)
    {

        val vf: ValueFunction = dataRow.value(dataFld.name);

        val strRep = vf() match
        {

            case x: IntValue =>
            {x.dataType.toString(x)}

            case x: GeneratedValue =>
            {x.dataType.toString(x)}

            case _ =>
            {"bad value"}

        }

        log("CsvDataRowHandler(" + dataSetSpec.name + ").writeColumn(" + dataFld + ") = [" + strRep + "]");
        fwriter.write(strRep);
        fwriter.flush;
    }

}
