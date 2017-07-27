package com.objectdynamics.tdg.builder.aggregates

import com.objectdynamics.tdg.generators.GeneratedValue

/**
  * Created by IntelliJ IDEA.
  * User: lcollins
  * Date: 8/17/11
  * Time: 11:11 PM
  * To change this template use File | Settings | File Templates.
  */

trait Aggregator {
  def acc(value: GeneratedValue, ctxt: Map[String, Any]): Map[String, Any];

  def value(ctxt: Map[String, Any]): GeneratedValue;
}

//
//class SumAgg extends Aggregator
//  {
//  def acc(value: GeneratedValue, ctxt: Map[String, Any]): Map[String, Any] = {
//
//    val soFar = ctxt.get("soFar") match {
//      case Some(bd: BigDecimal) => {
//        bd
//      }
//      case _ => {
//        BigDecimal(0);
//      }
//    }
//
//    val anv = value.asInstanceOf[AnonymousNumberValue];
//    val newValue = anv.value match {
//      case Some(x: BigDecimal) => {
//        x + soFar
//      }
//      case _ => {soFar}
//    }
//    val ret = ctxt + ("soFar" -> newValue);
//    ret;
//  };
//
//  def value(ctxt: Map[String, Any]) = {
//    ctxt.get("soFar") match {
//      case Some(bd: BigDecimal) => {
//        NumberValue( bd )
//      }
//      case _ => {
//        BigDecimal(0);
//      }
//    }
//  };
//  }