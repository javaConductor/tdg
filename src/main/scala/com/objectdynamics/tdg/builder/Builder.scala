package com.objectdynamics.tdg.builder

import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.generators._
import com.objectdynamics.tdg.model.{DefaultTestData, TestData}
import com.objectdynamics.tdg.parser.model._
import com.objectdynamics.tdg.schema.TestDataSchema

import scala.collection.mutable
import scalaz._

/**
  * Created by lee on 7/25/17.
  */
trait Builder {
  def build(buildRequest: BuildRequest,
            testDataSchema: TestDataSchema): BuilderException \/ TestData[DefaultDataSet]

  def buildDataSet(ctxt: BuilderContext,
                   treeRequest: TreeRequest, dss: IDataSetSpec): BuilderException \/ DefaultDataSet
}

class DefaultBuilder extends Builder {
  override def build(buildRequest: BuildRequest,
                     testDataSchema: TestDataSchema): BuilderException \/ DefaultTestData = {
    val builderContext: BuilderContext = createContext()
    val all = if (buildRequest.hasSpecifics) buildRequest.rootRequest :: buildRequest.specifics else List(buildRequest.rootRequest)

    //TODO return error messages
    if (!BuilderRequestValidator.validateBuildRequest(testDataSchema, buildRequest))
      -\/(new BuilderException("Bad request."))
    else
      \/-(all.foldRight(DefaultTestData(Seq.empty)) { (treq, td) => {
        handleTreeRequest(treq, testDataSchema, builderContext) match {
          case -\/(err) => throw err
          case \/-(ds: DefaultDataSet) => td.withDataSet(ds)
        }
      }
      })
  }

  def handleTreeRequest(tr: TreeRequest, testDataSchema: TestDataSchema, builderContext: BuilderContext): BuilderException \/ DefaultDataSet = {
    testDataSchema.dssMap(tr.dataSetName) match {
      case dss: IDataSetSpec => {
        buildDataSet(builderContext, tr, dss) match {
          case -\/(err) => -\/(err)
          case \/-(ds: DefaultDataSet) => \/-(ds)
          case _ => -\/(new BuilderException("Bad value from dssMap"))
        }
      }
      case _ => -\/(new BuilderException(s"No such data set: '${tr.dataSetName}'"))
    }
  }

  override def buildDataSet(builderContext: BuilderContext,
                            treeRequest: TreeRequest,
                            dss: IDataSetSpec): BuilderException \/ DefaultDataSet =
    DataSetBuilder.build(builderContext, treeRequest, dss)

  def createContext() = {
    var ctxtMap: mutable.Map[String, Any] = mutable.Map[String, Any]()

    def setCtxt(k: String, v: Any) = ctxtMap += (k -> v)

    def getCtxt(k: String): Option[Any] = ctxtMap.get(k)

    new BuilderContext {
      override val set: (String, Any) => Unit = setCtxt
      override val get: (String) => Option[Any] = getCtxt
    }
  }

}
