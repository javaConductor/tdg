package com.objectdynamics.tdg.builder

import com.objectdynamics.tdg.builder.model.{IDataSetSpec, ITestDataSchema}
import com.objectdynamics.tdg.parser.model.TreeRequest

/**
  * Created by Object Dynamics Inc.
  *
  * User: lcollins
  * Date: 9/29/11
  * Time: 2:22 AM
  *
  * Object Dynamics Inc. (c) 2001 - 2018
  *
  * Just add code =>
  */

trait BuilderRequestValidator {

  def validateBuildRequest(schema: ITestDataSchema, request: BuildRequest): Boolean

  def validateTreeRequest(schema: ITestDataSchema,
                          t: TreeRequest): Boolean

  def validateDataSetSpec(dss: IDataSetSpec,
                          t: TreeRequest): Boolean

  def validateCountVsConstraints(dss: IDataSetSpec,
                                 t: TreeRequest): Boolean
}

object BuilderRequestValidator extends BuilderRequestValidator {

  def validateBuildRequest(schema: ITestDataSchema, request: BuildRequest): Boolean = {
    (request.rootRequest :: request.specifics).forall(validateTreeRequest(schema, _))
  }

  def validateTreeRequest(schema: ITestDataSchema,
                          t: TreeRequest): Boolean = {

    if (schema.dataSetSpec(t.dataSetName) == None) false else true

  }

  def validateDataSetSpec(dss: IDataSetSpec,
                          t: TreeRequest): Boolean = {
    true
  }


  def validateCountVsConstraints(dss: IDataSetSpec,
                                 t: TreeRequest): Boolean = {
    true
  }

}
