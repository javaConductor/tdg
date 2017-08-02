/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.objectdynamics.tdg.generators

trait BuilderContext {
  val get: (String) => Option[Any]
  val set: (String, Any) => Unit
}
