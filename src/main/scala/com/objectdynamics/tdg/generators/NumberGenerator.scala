package com.objectdynamics.tdg.generators

import com.objectdynamics.tdg.builder.model.IDataField

trait NumberGenerator {
  def getMinMax(genParms: GeneratorParameters, dataField: IDataField): (BigDecimal, BigDecimal) = {

    val n: BigDecimal = genParms.min match {
      case scala.Some(m: BigDecimal) => m
      case None => dataField.min match {
        case scala.Some(dm: BigDecimal) => dm
        case None => new BigDecimal(new java.math.BigDecimal(0));
      }
    }

    val x: BigDecimal = genParms.max match {
      case scala.Some(m: BigDecimal) => m
      case None => dataField.max match {
        case scala.Some(dm: BigDecimal) => dm
        case None => new BigDecimal(new java.math.BigDecimal(0));
      }
    }
    (n, x)

  }
}