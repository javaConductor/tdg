package com.objectdynamics.tdg.parser.model

import java.lang.Boolean

case class TreeRequest(dataSetName: String,
                       rows: Long,
                       fieldConstraints: Map[String, FieldGenConstraints], // where clause
                       referenceName: Option[String],
                       symbolicName: Option[String],
                       unique: Boolean = false) {
}
