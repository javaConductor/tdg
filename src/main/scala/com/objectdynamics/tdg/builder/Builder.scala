package com.objectdynamics.tdg.builder

import java.util.Date

import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.generators.{Ctxt, GeneratedValue, IntValue}
import com.objectdynamics.tdg.model.{DataRow, TestData}
import com.objectdynamics.tdg.parser.model._
import com.objectdynamics.tdg.schema.TestDataSchema
import com.objectdynamics.tdg.spec.datatypes.{DataType, IntType}

import scala.collection.mutable
import scala.util.Random
import scalaz._

/**
  * Created by lee on 7/25/17.
  */
trait Builder {
  def build(buildRequest: BuildRequest,
            testDataSchema: TestDataSchema): BuilderException \/ ITestData = {
    val ctxt: Ctxt = createContext
    val testData: ITestData = TestData(Set())

    // get the data set if its in the schema
    val testDataOpt = testDataSchema.dssMap.get(buildRequest.rootRequest.dataSetName) match {
      case dss: IDataSetSpec => Some(buildDataSet(ctxt, testData, buildRequest.rootRequest, dss))
      case _ => None
    }

    testDataOpt match {
      case Some(x: BuilderException \/ ITestData) => x
      case _ => -\/(new BuilderException(s"No such data set: ${buildRequest.rootRequest.dataSetName}"))
    }

  }

  def buildDataSet(ctxt: Ctxt, testData: ITestData, treeRequest: TreeRequest, dss: IDataSetSpec): BuilderException \/ ITestData =
    DataSetBuilder.build(ctxt, testData, treeRequest, dss)

  def createContext() = {
    var ctxtMap: mutable.Map[String, Any] = mutable.Map[String, Any]()

    def setCtxt(k: String, v: Any) = ctxtMap += (k -> v)

    def getCtxt(k: String): Option[Any] = ctxtMap.get(k)

    new Ctxt {
      override val set: (String, Any) => Unit = setCtxt
      override val get: (String) => Option[Any] = getCtxt
    }
  }

}

trait DataSetBuilder {
  def build(ctxt: Ctxt, testData: ITestData, treeRequest: TreeRequest, dataSetSpec: IDataSetSpec): BuilderException \/ ITestData
}

object DataSetBuilder {
  def build(ctxt: Ctxt, testData: ITestData, treeRequest: TreeRequest, dataSetSpec: IDataSetSpec): BuilderException \/ ITestData =
    new DefaultDataSetBuilder().build(ctxt, testData, treeRequest, dataSetSpec)
}

class DefaultDataSetBuilder() extends DataSetBuilder {

  override def build(ctxt: Ctxt,
                     testData: ITestData,
                     treeRequest: TreeRequest,
                     dataSetSpec: IDataSetSpec): \/[BuilderException, ITestData] = {
    /// select the generators
    val constraints = treeRequest.fieldConstraints

    val generators = selectGenerators(dataSetSpec, constraints)
    generators match {
      case -\/(err) => -\/(err)
      case \/-(fgTpl) => {
        build(ctxt, testData, treeRequest, dataSetSpec, fgTpl)
      }
    }
  }

  def build(ctxt: Ctxt,
            testData: ITestData,
            treeRequest: TreeRequest,
            dataSetSpec: IDataSetSpec,
            generators: Seq[(IDataField, FieldGenerator[_])]): \/[BuilderException, ITestData] = {

    // initialize fg
    dataSetSpec.fields.foreach { fld =>
      val tpl = generators.find { tpl => tpl._1.name == fld.name }
      tpl match {
        case Some(gen) => {
          gen._2.init(ctxt,
            fld,
            dataSetSpec.name,
            treeRequest.rows,
            treeRequest.fieldConstraints.getOrElse(
              fld.name,
              FieldGenConstraints(fld.name, Set())
            )
          )
        }
      }
    }

    buildDataSet(ctxt, testData.dataSet(dataSetSpec.name).get, treeRequest.rows, generators) match {
      case -\/(err) => -\/(err)
      case \/-(dataSet) => \/-(testData + dataSet)
    }

  }

  def buildDataSet(ctxt: Ctxt,
                   dataSet: IDataSet,
                   nRows: Long,
                   generators: Seq[(IDataField, FieldGenerator[_])]): BuilderException \/ IDataSet = {

    if (nRows == 0) {
      \/-(dataSet)
    } else {
      buildDataRow(ctxt, dataSet.dataObjectSpec, generators) match {
        case -\/(err) => -\/(err)
        case \/-(dataRow) =>
          buildDataSet(ctxt, dataSet + dataRow, nRows - 1, generators)
      }
    }
  }

  def generateRow(ctxt: Ctxt,
                  dataSetSpec: IDataSetSpec,
                  dataRow: IDataRow,
                  generators: Seq[(IDataField, FieldGenerator[_])]): BuilderException \/ IDataRow = {

    generators match {
      case Nil => \/-(dataRow)
      case h :: t => {
        val df = h._1
        val fg = h._2
        fg.generate(ctxt, dataRow, df, dataSetSpec.name) match {
          case -\/(err) => -\/(err)
          case \/-(generatedValue) => generateRow(ctxt, dataSetSpec, dataRow + (df.name, generatedValue), t)
        }
      }
    }
  }

  def buildDataRow(ctxt: Ctxt,
                   dataSetSpec: IDataSetSpec,
                   generators: Seq[(IDataField, FieldGenerator[_])]): BuilderException \/ IDataRow =
    generateRow(ctxt, dataSetSpec, DataRow(dataSetSpec, "", Map.empty), generators)


  def selectGenerators(dataSetSpec: IDataSetSpec,
                       fieldGenConstraints: Map[String, FieldGenConstraints]): BuilderException \/ Seq[(IDataField, FieldGenerator[_])] = {
    val namesAndEither = dataSetSpec.fields.zip(dataSetSpec.fields.map {
      (df) =>
        selectGenerator(dataSetSpec, df, fieldGenConstraints.getOrElse(df.name, FieldGenConstraints(df.name, Set())))
    })

    namesAndEither.foldLeft((List[BuilderException](), List[(IDataField, FieldGenerator[_])]()))((acc, r) => {
      val dataField: IDataField = r._1
      r._2 match {
        case -\/(e) => (acc._1 :+ e, acc._2)
        case \/-(fg) => (acc._1, acc._2 :+ (dataField, fg))
      }
    }
    ) match {
      case (Nil, h :: t) => \/-(h :: t)
      case (h :: _, _) => -\/(h)
    }

  }

  def selectGenerator(dataSetSpec: IDataSetSpec,
                      dataField: IDataField,
                      fieldGenConstraints: FieldGenConstraints): BuilderException \/ FieldGenerator[_] = {
    FieldGeneratorList.generators.find { gen =>
      gen.canGenerate(dataField, fieldGenConstraints)
    } match {
      case Some(fg: FieldGenerator[_]) => \/-(fg)
      case None => -\/(new BuilderException(s"No Generator for dataType ${dataField.dataType.name} constraints $fieldGenConstraints"))
    }
  }
}

/** ***********************************************************************************************
  *
  * Generators
  *
  * ***********************************************************************************************/
trait FieldGenerator[I <: DataType[_]] {

  def init(ctxt: Ctxt, dataField: IDataField, dataSetName: String,
           nRows: Long, fldGenConstraints: FieldGenConstraints): Unit

  def generate(ctxt: Ctxt, dataRow: IDataRow, dataField: IDataField,
               dataSetName: String): BuilderException \/ GeneratedValue[_]

  def canGenerate(dataField: IDataField, fieldGenConstraints: FieldGenConstraints): Boolean
}


abstract class BaseGenerator[T](val name: String) extends FieldGenerator[T] {
  def prefix(name: String, dataSetName: String, fieldName: String) = s"$name:$dataSetName:$fieldName"
}

class IntegerGenerator extends BaseGenerator[IntType]("RandomInteger") {
  override def canGenerate(dataField: IDataField, fieldGenConstraints: FieldGenConstraints): Boolean = {
    dataField.dataType match {
      case t: IntType => {
        /// look thru the constraints for one unsupported
        fieldGenConstraints.fldGenSpecs.find {
          case BetweenSpec(_, _) => false
          case InSpec(_) => false
          case EqSpec(_) => false
          case _ => true
        } match {
          /// if an unsupported constraint is found then return false else true
          case Some(constraint) => {
            println(s"Generator $name does not support constraint: $constraint"); false
          }
          case _ => true

        }
      }
      /// if its not an Int then NO!
      case _ => false
    }
  }

  sealed trait Strategy

  object BetweenStrategy extends Strategy

  object ListStrategy extends Strategy

  object SameValueStrategy extends Strategy

  case class GenContext(min: BigInt, max: BigInt, list: Seq[BigInt], last: BigInt, strategy: Strategy) {
    def this() = this(BigInt(Int.MinValue), BigInt(Int.MaxValue), List.empty, 0)
  }

  override def init(ctxt: Ctxt,
                    dataField: IDataField,
                    dataSetName: String,
                    nRows: Long,
                    fldGenConstraints: FieldGenConstraints): Unit = {

    /// determine strategy
    val gc = fldGenConstraints.fldGenSpecs.foldLeft(GenContext) { (gc: GenContext, fldGenContraint) =>
      fldGenContraint match {
        case BetweenSpec(start, end) =>
          gc.copy(min = BigInt(start), max = BigInt(end), strategy = if (start == end) SameValueStrategy else BetweenStrategy)
        case InSpec(l) =>
          if (l.size == 1)
            gc.copy(min = BigInt(l.head), max = BigInt(l.head), strategy = SameValueStrategy)
          else
            gc.copy(list = l.map((i) => BigInt(i)), strategy = ListStrategy)
        case EqSpec(n) =>
          gc.copy(min = BigInt(n), max = BigInt(n), strategy = SameValueStrategy)
        case _ => gc
      }

    }
    val ctxtPrefix = prefix(name, dataSetName, dataField.name)
    ctxt.set(ctxtPrefix, gc)
  }

  val rand = new Random(new Date().getTime)

  def generateValue(gc: GenContext, ctxtPrefix: String,
                    dataRow: IDataRow, dataField: IDataField,
                    dataSetName: String): (GenContext, BuilderException \/ GeneratedValue[_]) = {

    gc.strategy match {
      case SameValueStrategy => (gc, \/-(IntValue(gc.min.toInt, Some(dataField.name))))
      case BetweenStrategy => {
        val value = IntValue(gc.min.toInt + (rand.nextDouble * (gc.min.toInt - gc.max.toInt)).toInt, Some(dataField.name))
        (gc, \/-(value))
      }
      case ListStrategy => {
        val min = 0
        val max = gc.list.size
        val idx = min + (rand.nextDouble * (max - min)).toInt
        val value = IntValue(gc.list(idx).toInt, Some(dataField.name))
        (gc, \/-(value))
      }
      case _ => (gc, -\/(new BuilderException(s"Unsupported strategy ${gc.strategy} in context $ctxtPrefix.")))
    }
  }

  override def generate(ctxt: Ctxt,
                        dataRow: IDataRow,
                        dataField: IDataField,
                        dataSetName: String): BuilderException \/ GeneratedValue[_] = {

    val ctxtPrefix = prefix(name, dataSetName, dataField.name)
    ctxt.get(ctxtPrefix) match {
      case None => -\/(new BuilderException(s"Context $ctxtPrefix not initialized ! ! !"))
      case Some(gc: GenContext) => {
        generateValue(gc, ctxtPrefix, dataRow, dataField, dataSetName) match {
          case (newGc, result) => {
            ctxt.set(ctxtPrefix, newGc)
            result
          }
        }
      }
    }
  }
}


object FieldGeneratorList {

  def generators: List[FieldGenerator[_]] = List(new IntegerGenerator)
}


