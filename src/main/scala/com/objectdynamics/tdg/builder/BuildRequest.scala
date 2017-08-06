package com.objectdynamics.tdg.builder

import com.objectdynamics.tdg.parser.model.TreeRequest

/**
  * Created by IntelliJ IDEA.
  * User: Lee
  * Date: Nov 22, 2010
  * Time: 12:40:14 AM
  */

case class BuildRequest(rootRequest: TreeRequest, nRows: Int, specifics: List[TreeRequest] = List.empty) {
  def hasSpecifics: Boolean = specifics.nonEmpty
}
