package com.objectdynamics.tdg.parser.model

/**
  * Created by IntelliJ IDEA.
  * User: Lee
  * Date: 2/3/11
  * Time: 1:14 AM
  * To change this template use File | Settings | File Templates.
  */

sealed trait FieldGenConstraint {
  def size(): Int
}

case class BetweenSpec(min: String, max: String) extends FieldGenConstraint {

  def size(): Int = max.toInt - min.toInt + 1
}

case class InSpec(data: Seq[String]) extends FieldGenConstraint {
  def size(): Int = data size
}

case class EqSpec(value: String) extends FieldGenConstraint {
  def size(): Int = 1
}

//case class LtSpec(value: String) extends FieldGenConstraint
//{
//    def size(): Int = 1;
//}
//
//case class GtSpec(value: String) extends FieldGenConstraint
//{
//    def size(): Int = 1;
//}
//
//case class LteSpec(value: String) extends LtSpec(value)
//{
//    def size(): Int = 1;
//}
//
//case class GteSpec(value: String) extends GtSpec(value)
//{
//    def size(): Int = 1;
//}

case class FieldGenConstraints(fieldName: String, fldGenSpecs: Set[FieldGenConstraint]) {
  def getBetweenSpec: Option[BetweenSpec] = {
    fldGenSpecs.find {
      case BetweenSpec(min: String, max: String) => true
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