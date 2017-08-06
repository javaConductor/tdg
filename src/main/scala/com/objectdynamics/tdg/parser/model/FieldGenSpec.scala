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

case class BetweenSpec(min: Long, max: Long) extends FieldGenConstraint

case class InSpec(data: Seq[Int]) extends FieldGenConstraint

case class EqSpec(value: String) extends FieldGenConstraint

case class FieldGenConstraints(fieldName: String, fldGenSpecs: Set[FieldGenConstraint]) {
  def getBetweenSpec: Option[BetweenSpec] = {
    fldGenSpecs.find {
      case BetweenSpec(min: Long, max: Long) => true
      case _ => false
    }.asInstanceOf[Option[BetweenSpec]]

  }

  def getInSpec: Option[InSpec] = {
    fldGenSpecs.find {
      case InSpec(data: List[String]) => true
      case _ => false
    }.asInstanceOf[Option[InSpec]]
  }

  def getEqSpec: Option[EqSpec] = {

    fldGenSpecs.find {
      case EqSpec(value: String) => true
      case _ => false
    }.asInstanceOf[Option[EqSpec]]
  }


}