package com.objectdynamics.tdg.generators

import java.util.Date

import com.objectdynamics.tdg.builder.BuilderException
import com.objectdynamics.tdg.builder.model.{DataRow, IDataField}
import com.objectdynamics.tdg.parser.model.{BetweenSpec, EqSpec, FieldGenConstraints, InSpec}
import com.objectdynamics.tdg.spec.datatypes.ScalaInt

import scala.util.Random
import scalaz.{-\/, \/, \/-}

/** ***********************************************************************************************
  *
  * Generators
  *
  * ***********************************************************************************************/
class IntegerGenerator extends BaseGenerator("RandomInteger") {
  type U = Int

  override def canGenerate(dataField: IDataField, fieldGenConstraints: Option[FieldGenConstraints]): Boolean = {
    dataField.dataType match {
      case _: ScalaInt => {
        if (fieldGenConstraints.isEmpty)
          true
        else {

          /// look thru the constraints for one unsupported
          fieldGenConstraints.get.fldGenSpecs.find {
            case BetweenSpec(_, _) => false
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
      /// if its not an Int then NO!
      case _ => false
    }
  }

  sealed trait Strategy

  object NoStrategy extends Strategy

  object BetweenStrategy extends Strategy

  object ListStrategy extends Strategy

  object SameValueStrategy extends Strategy

  object GenContext {
    def instance: GenContext = GenContext(BigInt(-1), BigInt(-1), List.empty, BigInt(-1), NoStrategy)
  }

  case class GenContext(min: BigInt, max: BigInt, list: Seq[BigInt], last: BigInt, strategy: Strategy) {
    def this() = this(BigInt(-1), BigInt(-1), List.empty, BigInt(-1), NoStrategy)
  }

  override def init(ctxt: BuilderContext,
                    dataField: IDataField,
                    dataSetName: String,
                    nRows: Long,
                    fldGenConstraints: FieldGenConstraints): Unit = {


    // we currently only support one of the fieldGenConstraints
    /// determine strategy
    val gc: GenContext = fldGenConstraints.fldGenSpecs.head match {
        case BetweenSpec(min:Long , max:Long ) =>
          GenContext(min.toInt, max.toInt, List.empty, BigInt(-1), BetweenStrategy)

        case InSpec(l: List[Int]) =>
          if (l.size == 1)
            GenContext(BigInt(l.head), BigInt(l.head), List.empty, BigInt(-1), strategy = SameValueStrategy)
          else
            GenContext(BigInt(-1), BigInt(-1),l.map((i) => BigInt(i)), BigInt(-1), strategy = ListStrategy)

        case EqSpec(n) =>
          GenContext(min = BigInt(n), max = BigInt(n), List.empty, BigInt(-1), strategy = SameValueStrategy)

        case _ => GenContext(BigInt(0), BigInt(1000), List.empty, BigInt(-1), BetweenStrategy)
      }

    val ctxtPrefix = prefix(name, dataSetName, dataField.name)
    ctxt.set(ctxtPrefix, gc)
  }

  val rand = new Random(new Date().getTime)

  def generateValue(gc: GenContext, ctxtPrefix: String,
                    dataRow: DataRow, dataField: IDataField,
                    dataSetName: String): (GenContext, BuilderException \/ GeneratedValue) = {

    gc.strategy match {
      case SameValueStrategy => (gc, \/-(IntValue(gc.min.toInt, dataField.name)))
      case BetweenStrategy => {
        val range = (gc.max - gc.min).abs
        val x = rand.nextDouble
        val offset = x * range.toLong
        val value = IntValue((gc.min.toLong + (offset)).toInt,
          dataField.name)
        (gc, \/-(value))
      }
      case ListStrategy => {
        val min = 0
        val max = gc.list.size
        val idx = min + (rand.nextDouble * (max - min)).toInt
        val value = IntValue(gc.list(idx).toInt, dataField.name)
        (gc, \/-(value))
      }
      case _ => (gc, -\/(new BuilderException(s"Unsupported strategy ${gc.strategy} in context $ctxtPrefix.")))
    }
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
}
