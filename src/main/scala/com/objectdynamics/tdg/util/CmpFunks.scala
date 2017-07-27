package com.objectdynamics.tdg.util

import java.util._;

trait CmpFunks {

  implicit def stringEqAny(s: String, a: Any): Boolean = {
    s == a.toString
  }

  implicit def doubleEqAny(d: Double, a: Any): Boolean = {
    d == a.toString.toDouble
  }

  implicit def dateEqAny(dt: java.util.Date, a: Any): Boolean = {
    dt == a.asInstanceOf[Date]
  }

  implicit def bigDecEqAny(bd: BigDecimal, a: Any): Boolean = {
    bd == a.asInstanceOf[BigDecimal]
  }
}