/*
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 3/3/11
 * Time: 12:48 AM
 */
package com.objectdynamics.tdg.schema

import com.objectdynamics.tdg.builder.model.ITestDataSchema

trait MetadataLoader {
  def fromSource[A](source: A): ITestDataSchema;

  def fromText(text: String): ITestDataSchema;

  def fromFile(fileName: String): ITestDataSchema;
}