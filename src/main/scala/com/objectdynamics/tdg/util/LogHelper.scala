package com.objectdynamics.tdg.util

trait LogHelper
{
    val loggerName = this.getClass.getName
    lazy val logger = org.apache.log4j.Logger.getLogger(loggerName)
}
