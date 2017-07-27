package com.objectdynamics.tdg.spec

import com.objectdynamics.tdg.parser.model.FieldGenConstraints

case class Relationship(dataSetName: String,
                        fieldMap: Map[String, String],
                        fieldConstraints: Map[String, FieldGenConstraints],
                        defaultDisbursementSpec: Option[DisbursementSpec],
                        active: Boolean) {

}