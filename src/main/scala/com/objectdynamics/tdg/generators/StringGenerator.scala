package com.objectdynamics.tdg.generators

import java.util.Date

import com.objectdynamics.tdg.builder.BuilderException
import com.objectdynamics.tdg.builder.model.{DataRow, IDataField}
import com.objectdynamics.tdg.parser.model.{EqSpec, FieldGenConstraints, InSpec}
import com.objectdynamics.tdg.spec.datatypes.ScalaString

import scala.util.Random
import scalaz.{-\/, \/, \/-}

/** ***********************************************************************************************
  *
  * Generators
  *
  * ***********************************************************************************************/
class StringGenerator extends BaseGenerator("StringGen") {
  type U = String
  override def canGenerate(dataField: IDataField, fieldGenConstraints: Option[FieldGenConstraints]): Boolean = {
    dataField.dataType match {
      case _: ScalaString => {
        if(fieldGenConstraints.isEmpty)
          false /// string needs some guidance
        else {
        /// look thru the constraints for one unsupported
        fieldGenConstraints.get.fldGenSpecs.find {
          case InSpec(_) => false
          case EqSpec(_) => false
          case _ => true
        } match {
          /// if an unsupported constraint is found then return false else true
          case Some(constraint) => {
            println(s"Generator $name does not support constraint: $constraint");
            false
          }
          case _ => true
        }
      }
      }
      /// if its not an String then NO!
      case _ => false
    }
  }

  sealed trait Strategy
  object NoStrategy extends Strategy
  object ListStrategy extends Strategy
  object SameValueStrategy extends Strategy

  object GenContext {
    def instance:GenContext = GenContext(List.empty, -1, NoStrategy)
  }

  case class GenContext(list: Seq[U], lastIdx: Int, strategy: Strategy) {
  }

  override def init(ctxt: BuilderContext,
                    dataField: IDataField,
                    dataSetName: String,
                    nRows: Long,
                    fldGenConstraints: FieldGenConstraints): Unit = {

    /// determine strategy
val gc:GenContext = if(fldGenConstraints.fldGenSpecs.isEmpty)
    GenContext ( List.empty, -1,ListStrategy )
 else
    fldGenConstraints.fldGenSpecs.foldLeft(GenContext.instance) ({
      (gc: GenContext, fldGenContraint) =>
        fldGenContraint match {
          case InSpec(l:List[U]) =>
            if (l.size == 1)
              gc.copy( list = l, strategy = SameValueStrategy)
            else
              gc.copy(list = l, strategy = ListStrategy)
          case EqSpec(n) =>
            gc.copy( list = List(n), strategy = SameValueStrategy)
          case _ => gc
        }
    });

    val ctxtPrefix = prefix(name, dataSetName, dataField.name)
    ctxt.set(ctxtPrefix, gc)
  }

  val rand = new Random(new Date().getTime)

  def generateValue(gc: GenContext, ctxtPrefix: String,
                    dataRow: DataRow, dataField: IDataField,
                    dataSetName: String): (GenContext, BuilderException \/ GeneratedValue[U]) = {

    gc.strategy match {
      case SameValueStrategy => (gc, \/-(StringValue(gc.list.head, dataField.name)))
      case ListStrategy => {
        val min = 0
        val max = gc.list.size
        val idx = min + (rand.nextDouble * (max - min )).toInt
        val value = StringValue(gc.list(idx), dataField.name)
        (gc, \/-(value))
      }
      case _ => (gc, -\/(new BuilderException(s"Unsupported strategy ${gc.strategy} in context $ctxtPrefix.")))
    }
  }

  override def generate(ctxt: BuilderContext,
                        dataRow: DataRow,
                        dataField: IDataField,
                        dataSetName: String): BuilderException \/ GeneratedValue[U] = {

    val ctxtPrefix = prefix(name, dataSetName, dataField.name)
    ctxt.get(ctxtPrefix) match {
      case Some(gc: GenContext) => {
        generateValue(gc, ctxtPrefix, dataRow, dataField, dataSetName) match {
          case (newGc, result) => {
            ctxt.set(ctxtPrefix, newGc)
            result
          }
        }
      }
      case _ => -\/(new BuilderException(s"Context $ctxtPrefix not initialized ! ! !"))
    }
  }
}
