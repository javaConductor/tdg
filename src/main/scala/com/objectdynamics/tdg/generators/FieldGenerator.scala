package com.objectdynamics.tdg.generators

import com.objectdynamics.tdg.builder.BuilderException
import com.objectdynamics.tdg.builder.model.{DataRow, IDataField}
import com.objectdynamics.tdg.parser.model.FieldGenConstraints
import scalaz.\/

/**
  * Created by lee on 8/4/17.
  */
trait FieldGenerator {
  type I

  def init(ctxt: BuilderContext, dataField: IDataField, dataSetName: String,
           nRows: Long, fldGenConstraints: FieldGenConstraints): Unit

  def generate(ctxt: BuilderContext, dataRow: DataRow, dataField: IDataField,
               dataSetName: String): BuilderException \/ GeneratedValue

  def canGenerate(dataField: IDataField, fieldGenConstraints: Option[FieldGenConstraints]): Boolean
}

abstract class BaseGenerator(val name: String) extends FieldGenerator {
  def prefix(name: String, dataSetName: String, fieldName: String) = s"$name:$dataSetName:$fieldName"
}

object FieldGenerator {

}


object FieldGeneratorList {
  def generators: Seq[FieldGenerator] = List(
    new IntegerGenerator(),
    new StringGenerator(),
    new DoubleGenerator()
  )
}
