package com.objectdynamics.tdg.util

/**
  * Created by IntelliJ IDEA.
  * User: Lee
  * Date: 12/30/10
  * Time: 12:27 AM
  * To change this template use File | Settings | File Templates.
  */

object TypeHelper {

  def stringToBool(s: String): Boolean = {

    s.toLowerCase() match {
      case "true" => true
      case "t" => true
      case "yes" => true
      case "y" => true
      case _ => false
    }
  }
}