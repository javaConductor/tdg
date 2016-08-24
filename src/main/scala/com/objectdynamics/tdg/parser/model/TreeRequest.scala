package com.objectdynamics.tdg.parser.model

import java.lang.Boolean

case class TreeRequest(dataSetName: String,
                       rows: Long,
                       fieldConstraints: Map[String, FieldGenConstraints], // where clause
                       referenceName: Option[String],
                       symbolicName: Option[String],
                       subTrees: List[TreeRequest], // with clause
                       unique:    Boolean = false)
{
    def isAReference = referenceName match {
        case Some(s: String) =>
        {true}
        case None =>
        {false}
    }

    def hasSymbolicName = symbolicName match {
        case Some(s: String) =>
        {true}
        case None =>
        {false}
    }

}
