package com.objectdynamics.tdg.util

trait LogHelper {
  lazy val logger = org.apache.log4j.Logger.getLogger(loggerName)
  val loggerName = this.getClass.getName
}
