package com.objectdynamics.tdg.builder.delegate

import com.objectdynamics.tdg.builder.model.IDataRow

/**
  * Created by IntelliJ IDEA.
  * User: Lee
  * Date: Dec 8, 2010
  * Time: 9:21:31 PM
  * To change this template use File | Settings | File Templates.
  */

trait Distribution {
  val dtype: String;

  def process(partialRow: IDataRow): Distribution;

  def needsRow(pRow: IDataRow): Boolean;

  def complete: Boolean;

}