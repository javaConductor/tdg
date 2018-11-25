package com.objectdynamics.tdg.parser.model

/**
  * Created by IntelliJ IDEA.
  * User: Lee
  * Date: 2/3/11
  * Time: 1:14 AM
  * To change this template use File | Settings | File Templates.
  */

sealed trait FieldGenConstraint {
  //  def size(): Int
}

@Deprecated
case class BetweenSpec(min: Long, max: Long) extends FieldGenConstraint

case class BetweenValuesSpec[T](min: T, max: T) extends FieldGenConstraint

case class InSpec[T](data: Seq[T]) extends FieldGenConstraint

case class EachSpec[T](data: Seq[T]) extends FieldGenConstraint

case class EqSpec[T](value: T) extends FieldGenConstraint

case class FieldGenConstraints(fieldName: String, fldGenSpecs: Set[FieldGenConstraint]) {
  def getBetweenSpec: Option[BetweenSpec] = {
    fldGenSpecs.find {
      case BetweenSpec(min: Long, max: Long) => true
      case _ => false
    }.asInstanceOf[Option[BetweenSpec]]

  }

  def getBetweenValuesSpec[T]: Option[BetweenValuesSpec[T]] = {
    fldGenSpecs.find {
      case BetweenValuesSpec(min: T, max: T) => true
      case _ => false
    }.asInstanceOf[Option[BetweenValuesSpec[T]]]

  }

  def getInSpec[T]: Option[InSpec[T]] = {
    fldGenSpecs.find {
      case InSpec(data: List[T]) => true
      case _ => false
    }.asInstanceOf[Option[InSpec[T]]]
  }

  def getEachSpec[T]: Option[EachSpec[T]] = {
    fldGenSpecs.find {
      case EachSpec(data: List[T]) => true
      case _ => false
    }.asInstanceOf[Option[EachSpec[T]]]
  }

  def getEqSpec[T]: Option[EqSpec[T]] = {
    fldGenSpecs.find {
      case EqSpec(value: T) => true
      case _ => false
    }.asInstanceOf[Option[EqSpec[T]]]
  }

}