package com.objectdynamics.tdg.generators

import java.util.Date

import com.objectdynamics.tdg.builder.BuilderException
import com.objectdynamics.tdg.builder.model.{DataRow, IDataField}
import com.objectdynamics.tdg.parser.model._
import com.objectdynamics.tdg.spec.datatypes.{ScalaDouble, ScalaFloat}

import scala.util.Random
import scalaz.{-\/, \/, \/-}

/** ***********************************************************************************************
  *
  * Generators
  *
  * ***********************************************************************************************/
class DoubleGenerator extends BaseGenerator("RandomDouble") {
  type U = Float
  val rand = new Random(new Date().getTime)

  override def canGenerate(dataField: IDataField, fieldGenConstraints: Option[FieldGenConstraints]): Boolean = {

    val isDouble: Boolean = dataField.dataType match {
      case _: ScalaDouble => {
        true
      }
      case _: ScalaFloat => {
        true
      }
      case _ => false
    }

    if (!isDouble) false else {
      /// look thru the constraints for one unsupported
      fieldGenConstraints.get.fldGenSpecs.find {
        case BetweenSpec(_, _) => false
        case BetweenValuesSpec(_, _) => false
        case InSpec(_) => false
        case EqSpec(_) => false
        case EachSpec(_) => false
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

  override def init(ctxt: BuilderContext,
                    dataField: IDataField,
                    dataSetName: String,
                    nRows: Long,
                    fldGenConstraints: FieldGenConstraints): Unit = {
    // we currently only support one of the fieldGenConstraints
    /// determine strategy
    val gc: GenContext = fldGenConstraints.fldGenSpecs.head match {
      case BetweenValuesSpec(min: Double, max: Double) =>
        GenContext(min.toDouble, max.toDouble, List.empty, Double.NaN, BetweenStrategy)

      case EachSpec(l: List[Double]) =>
        if (l.size == 1)
          GenContext((l.head), (l.head), List.empty, Double.NaN, strategy = SameValueStrategy)
        else
          GenContext((l.head), (l.head), l, (l.size).toDouble, strategy = EachStrategy)

      case InSpec(l: List[Double]) =>
        if (l.size == 1)
          GenContext((l.head), (l.head), List.empty, Double.NaN, strategy = SameValueStrategy)
        else
          GenContext((l.head), (l.head), l, Double.NaN, strategy = ListStrategy)

      case EqSpec(n: Double) =>
        GenContext(min = n, max = n, List.empty, Double.NaN, strategy = SameValueStrategy)

      case _ => GenContext(0, 1000, List.empty, Double.NaN, BetweenStrategy)
    }

    val ctxtPrefix = prefix(name, dataSetName, dataField.name)
    ctxt.set(ctxtPrefix, gc)
  }

  override def generate(ctxt: BuilderContext,
                        dataRow: DataRow,
                        dataField: IDataField,
                        dataSetName: String): BuilderException \/ GeneratedValue = {

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

  def generateValue(gc: GenContext, ctxtPrefix: String,
                    dataRow: DataRow, dataField: IDataField,
                    dataSetName: String): (GenContext, BuilderException \/ GeneratedValue) = {

    gc.strategy match {
      case SameValueStrategy => (gc, \/-(DoubleValue(gc.min.toInt, dataField.name)))
      case BetweenStrategy => {
        val range = (gc.max - gc.min).abs
        val x = rand.nextDouble
        val offset = x * range.toLong
        val value = DoubleValue((gc.min.toLong + (offset)),
          dataField.name)
        (gc, \/-(value))
      }
      case EachStrategy => {
        val max = gc.list.size - 1
        // gc.last should contain the last index used
        val idx = if (gc.last.toInt >= max) 0 else gc.last.toInt + 1
        val value = DoubleValue(gc.list(idx), dataField.name)
        (gc.copy(last = (idx)), \/-(value))
      }
      case ListStrategy => {
        val min = 0
        val max = gc.list.size
        val idx = min + (rand.nextDouble * (max - min)).round.toInt
        val value = DoubleValue(gc.list(idx).toInt, dataField.name)
        (gc, \/-(value))
      }
      case _ => (gc, -\/(new BuilderException(s"Unsupported strategy ${gc.strategy} in context $ctxtPrefix.")))
    }
  }

  case class GenContext(min: Double, max: Double, list: Seq[Double], last: Double, strategy: Strategy) {
    def this() = this(Double.NaN, Double.NaN, List.empty, Double.NaN, NoStrategy)
  }

  object GenContext {
    def instance: GenContext = GenContext(Double.NaN, Double.NaN, List.empty, Double.NaN, NoStrategy)
  }
}
