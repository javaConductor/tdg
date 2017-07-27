package com.objectdynamics.tdg.generators

import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.parser.model.FieldGenConstraints

import scala.collection.immutable._

/*
A <: ITestDataSchema,
B <: IDataSetSpec,
C <: IDataField,
R <: IDataSetBuilder,
X <: ITestData,
Y <: IDataSet,
Z <: IDataRow]
 */

class FieldGenList(funcList: scala.List[FieldGeneratorFunction]) {
  val funcMap: Map[String, FieldGeneratorFunction] = ((
    funcList map {
      fgf: FieldGeneratorFunction => (fgf.name -> fgf)
    }) toMap) ++ defaultGenerators;

  def this() = {
    this(List[FieldGeneratorFunction]());
  }

  //        ListHelper.listToMap[String, FieldGeneratorFunction](funcList,
  //                                                             {fgf: FieldGeneratorFunction => fgf.name});

  def defaultGenerators(): Map[String, FieldGeneratorFunction] = {
    val fgfList: List[FieldGeneratorFunction] = List[FieldGeneratorFunction](new GenerateIntegerAutoIncrement,
      new RandomInteger,
      //new ForeignRefGenerator,
      new RandomFloat,
      new RotateString);
    fgfList map {
      fgf: FieldGeneratorFunction => (fgf.name -> fgf)
    } toMap
  }

  def forField(fld: IDataField, fieldConstraints: Option[FieldGenConstraints]): Option[FieldGeneratorFunction] = {

    fld.generatorName match {
      case Some(name) => findByName(name);
      case None => findFirstPossibleGenerator(fld, fieldConstraints);
    }
  }

  def findByName(name: String): Option[FieldGeneratorFunction] = {
    funcMap.get(name)
  }

  def findFirstPossibleGenerator(fld: IDataField, fieldConstraints: Option[FieldGenConstraints]): Option[FieldGeneratorFunction] = {
    funcMap.values.find {
      fgf: FieldGeneratorFunction =>
        fgf.canGenerateField(fld, fieldConstraints);
    }
  }

  def +(ffunc: FieldGeneratorFunction): FieldGenList = {
    new FieldGenList(ffunc :: funcList);
  }


}