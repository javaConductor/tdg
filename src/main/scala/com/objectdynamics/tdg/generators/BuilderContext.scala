/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.objectdynamics.tdg.generators

class BuilderContext extends scala.collection.mutable.HashMap[String, Any]

trait Ctxt {
  val get: (String) => Option[Any]
  val set: (String, Any) => Unit
}
