package com.objectdynamics.tdg.builder

import com.objectdynamics.tdg.builder.model.IDataRow
import com.objectdynamics.tdg.parser.model.FieldGenConstraints

/**
  * Created by IntelliJ IDEA.
  * User: lcollins
  * Date: 8/11/11
  * Time: 1:00 AM
  * To change this template use File | Settings | File Templates.
  */

trait ObjectHolder {

  def +=(obj: IDataRow) {}

  def data: List[IDataRow] = {
    Nil
  }

  def find(constraints: Map[String, FieldGenConstraints]): List[IDataRow];


}