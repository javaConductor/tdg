package com.objectdynamics.tdg.generators

import java.util.Date

import com.objectdynamics.tdg.builder.BuilderException
import com.objectdynamics.tdg.builder.model.{DataRow, IDataField}
import com.objectdynamics.tdg.parser.model.{BetweenSpec, EqSpec, FieldGenConstraints, InSpec}
import com.objectdynamics.tdg.spec.datatypes.ScalaInt
import com.sun.org.apache.xalan.internal.xsltc.compiler.util.IntType

import scala.util.Random
import scalaz.{-\/, \/, \/-}

/** ***********************************************************************************************
  *
  * Generators
  *
  * ***********************************************************************************************/
class IntegerGenerator extends BaseGenerator("RandomInteger") {
  type I = IntType
  override def canGenerate(dataField: IDataField, fieldGenConstraints: FieldGenConstraints): Boolean = {
    dataField.dataType match {
      case _: ScalaInt => {
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
  object NoStrategy extends Strategy
  object BetweenStrategy extends Strategy
  object ListStrategy extends Strategy
  object SameValueStrategy extends Strategy

  object GenContext {
    def instance:GenContext = GenContext(BigInt(-1), BigInt(-1), List.empty, BigInt(-1), NoStrategy)
  }

  case class GenContext(min: BigInt, max: BigInt, list: Seq[BigInt], last: BigInt, strategy: Strategy) {
    def this() = this(BigInt(-1), BigInt(-1), List.empty, BigInt(-1), NoStrategy)
  }

  override def init(ctxt: BuilderContext,
                    dataField: IDataField,
                    dataSetName: String,
                    nRows: Long,
                    fldGenConstraints: FieldGenConstraints): Unit = {

    /// determine strategy
val gc:GenContext = if(fldGenConstraints.fldGenSpecs.isEmpty)
  //GenContext ( BigInt(Int.MinValue + 1), BigInt(Int.MaxValue - 1), List.empty, BigInt(-1), BetweenStrategy)
  GenContext ( BigInt(0), BigInt(1000), List.empty, BigInt(-1), BetweenStrategy)
 else
    fldGenConstraints.fldGenSpecs.foldLeft(GenContext.instance) ({
      (gc: GenContext, fldGenContraint) =>
        fldGenContraint match {
          case BetweenSpec(start, end) =>
            gc.copy(min = BigInt(start),
              max = BigInt(end),
              strategy = if (start == end) SameValueStrategy else BetweenStrategy)
          case InSpec(l) =>
            if (l.size == 1)
              gc.copy( min = BigInt(l.head), max = BigInt(l.head), strategy = SameValueStrategy)
            else
              gc.copy(list = l.map((i) => BigInt(i)), strategy = ListStrategy)
          case EqSpec(n) =>
            gc.copy(min = BigInt(n), max = BigInt(n), strategy = SameValueStrategy)
          case _ => gc
        }
    });

    val ctxtPrefix = prefix(name, dataSetName, dataField.name)
    ctxt.set(ctxtPrefix, gc)
  }

  val rand = new Random(new Date().getTime)

  def generateValue(gc: GenContext, ctxtPrefix: String,
                    dataRow: DataRow, dataField: IDataField,
                    dataSetName: String): (GenContext, BuilderException \/ GeneratedValue[_]) = {

    gc.strategy match {
      case SameValueStrategy => (gc, \/-(IntValue(gc.min.toInt, dataField.name)))
      case BetweenStrategy => {
        val range = ( gc.max - gc.min).abs
        val x= rand.nextDouble
        val offset = x * range.toLong
        val value = IntValue( (gc.min.toLong + (offset)).toInt,
          dataField.name)
        (gc, \/-(value))
      }
      case ListStrategy => {
        val min = 0
        val max = gc.list.size
        val idx = min + (rand.nextDouble * (max - min )).toInt
        val value = IntValue(gc.list(idx).toInt, dataField.name)
        (gc, \/-(value))
      }
      case _ => (gc, -\/(new BuilderException(s"Unsupported strategy ${gc.strategy} in context $ctxtPrefix.")))
    }
  }

  override def generate(ctxt: BuilderContext,
                        dataRow: DataRow,
                        dataField: IDataField,
                        dataSetName: String): BuilderException \/ GeneratedValue[_] = {

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
