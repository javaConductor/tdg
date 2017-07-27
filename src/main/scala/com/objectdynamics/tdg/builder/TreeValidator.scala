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
  * Object Dynamics Inc. (c) 2001 - 2011
  *
  * Just add code =>
  */

trait TreeValidator {

  def validateTreeRequest(schema: ITestDataSchema,
                          t: TreeRequest): Boolean = {

    if (schema.dataSetSpec(t.dataSetName) == None) {
      return false
    }
    true;

  }

  def validateDataSetSpec(dss: IDataSetSpec,
                          t: TreeRequest): Boolean = {
    true
  }


  def validateCountVsContraints(dss: IDataSetSpec,
                                t: TreeRequest): Boolean = {
    true
  }

}
