package com.objectdynamics.tdg.builder

import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.generators.{BuilderContext, FieldGenerator, FieldGeneratorList}
import com.objectdynamics.tdg.parser.model.{FieldGenConstraints, TreeRequest}

import scala.annotation.tailrec
import scalaz.{-\/, GlorifiedTuple, \/, \/-}

/**
  * Created by lee on 8/6/17.
  */

trait DataSetBuilder {
  def build(builderContext: BuilderContext,
            treeRequest: TreeRequest,
            dataSetSpec: IDataSetSpec): BuilderException \/ DefaultDataSet
}

object DataSetBuilder {
  def build(ctxt: BuilderContext, treeRequest: TreeRequest, dataSetSpec: IDataSetSpec): BuilderException \/ DefaultDataSet =
    new DefaultDataSetBuilder().build(ctxt, treeRequest, dataSetSpec)
}

class DefaultDataSetBuilder() extends DataSetBuilder {

  override def build(ctxt: BuilderContext,
                     treeRequest: TreeRequest,
                     dataSetSpec: IDataSetSpec): \/[BuilderException, DefaultDataSet] = {
    /// select the generators
    val constraints = treeRequest.fieldConstraints
    val generators = selectGenerators(dataSetSpec, constraints)
    generators match {
      case -\/(err) => -\/(err)
      case \/-(fgTpl) => build(ctxt, treeRequest, dataSetSpec, fgTpl)
    }
  }

  def build(ctxt: BuilderContext,
            treeRequest: TreeRequest,
            dataSetSpec: IDataSetSpec,
            generators: Seq[(IDataField, FieldGenerator)]): GlorifiedTuple[BuilderException, DefaultDataSet] = {

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
        case _ => {}
      }
    }
    buildDataSet(ctxt, new DefaultDataSet(dataSetSpec, List[DataRow]()), treeRequest.rows, generators)
  }

  @tailrec final def buildDataSet(ctxt: BuilderContext,
                                  dataSet: DefaultDataSet,
                                  nRows: Long,
                                  generators: Seq[(IDataField, FieldGenerator)]): BuilderException \/ DefaultDataSet = {

    if (nRows == 0) {
      \/-(dataSet)
    } else {
      buildDataRow(ctxt, dataSet.dataObjectSpec, generators) match {
        case -\/(err) => -\/(err)
        case \/-(dataRow:DefaultDataRow) =>
          buildDataSet(ctxt, dataSet + dataRow, nRows - 1, generators)
        case _ => -\/( new BuilderException(s"Internal error: Bad value from buildDataRoe( $ctxt, ${dataSet.dataObjectSpec}, $generators ) "))
      }
    }
  }


  /**
    * Internal recursive function for generating values for the specified dataSetSpec
    * @param ctxt
    * @param dataSetSpec
    * @param dataRow
    * @param generators Generators to use for each field
    * @return
    */
  @tailrec private def generateRow(ctxt: BuilderContext,
                  dataSetSpec: IDataSetSpec,
                  dataRow: DataRow,
                  generators: Seq[(IDataField, FieldGenerator)]) : BuilderException \/ DataRow = {

    generators match {
      /// Handle base case
      case Nil => {
        \/-(dataRow)
      }
      /// dataRow is not complete so we generate the next value
      case h :: t => {
        val df = h._1
        val fg = h._2
        // generate the value
        fg.generate(ctxt, dataRow, df, dataSetSpec.name) match {
            /// if we hit an error return it
          case -\/(err) => -\/(err)
            /// if we generated a value then lets do the next one
          case \/-(generatedValue) => generateRow(ctxt, dataSetSpec, dataRow + generatedValue, t)
        }
      }
    }
  }

  def buildDataRow(ctxt: BuilderContext,
                   dataSetSpec: IDataSetSpec,
                   generators: Seq[(IDataField, FieldGenerator)]): BuilderException \/ DataRow =
    generateRow(ctxt, dataSetSpec, DefaultDataRow(dataSetSpec, "", Map.empty), generators)

  /**
    * Returns a Seq of dataField->fieldGenerator pairs for a dataSetSpec
    * Generators must be able to handle the fieldGenConstraints
    *
    * @param dataSetSpec
    * @param fieldGenConstraints
    * @return \/(BuilderException, Seq[(IDataField, FieldGenerator)])
    */
  def selectGenerators(dataSetSpec: IDataSetSpec,
                       fieldGenConstraints: Map[String, FieldGenConstraints])
  : BuilderException \/ Seq[(IDataField, FieldGenerator)] = {
    // Find willing generators and zip it with the associated DataField
    val fieldsAndGenerators = dataSetSpec.fields.zip(dataSetSpec.fields.map {
      (df) =>
        selectGenerator(dataSetSpec, df, fieldGenConstraints.get(df.name))
    })

    // fold over the fld/generators Seq extracting errors
    val startValue = (List[BuilderException](), List[(IDataField, FieldGenerator)]())
    fieldsAndGenerators.foldLeft(startValue)((acc, r) => {
      val dataField: IDataField = r._1
      r._2 match {
        case -\/(e) => (e :: acc._1, acc._2)
        case \/-(fg) => (acc._1, (dataField -> fg) :: acc._2 )
      }
    }
    ) match {
      case (Nil, h :: t) => \/-(h :: t)
      case (Nil, Nil) => -\/(new BuilderException(s"Internal Error: No Generators found for dataSetSpec: $dataSetSpec"))
      case (errors :: _, _) => -\/(errors)
    }

  }

  /**
    * Selects a Generator for the specified DataField capable of specified constraints
    *
    * @param dataSetSpec
    * @param dataField
    * @param fieldGenConstraints
    *
    * @return  BuilderException \/ FieldGenerator
    */
  def selectGenerator(dataSetSpec: IDataSetSpec,
                      dataField: IDataField,
                      fieldGenConstraints: Option[FieldGenConstraints]): BuilderException \/ FieldGenerator = {
    FieldGeneratorList.generators.find { gen =>
      gen.canGenerate(dataField, fieldGenConstraints)
    } match {
      case Some(fg: FieldGenerator) => \/-(fg)
      case None => -\/(new BuilderException(s"No Generator for dataType ${dataField.dataType.name} constraints $fieldGenConstraints"))
    }
  }
}
