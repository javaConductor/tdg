package com.objectdynamics.tdg.builder

import com.objectdynamics.tdg.parser.model.TreeRequest

/**
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: Nov 22, 2010
 * Time: 12:40:14 AM
 * To change this template use File | Settings | File Templates.
 */

case class BuildRequest(rootObject: String, nRows: Int, specifics: List[TreeRequest] = List.empty)
{
    //    def specifics: List[TreeRequest] = _specifics

    def hasSpecifics: Boolean = !specifics.isEmpty;

}