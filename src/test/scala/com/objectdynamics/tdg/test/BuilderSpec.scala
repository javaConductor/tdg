package com.objectdynamics.tdg.test

import com.objectdynamics.tdg.builder.model.ScalaInt
import com.objectdynamics.tdg.builder.{BuildRequest, DefaultBuilder}
import com.objectdynamics.tdg.parser.model.{BetweenSpec, FieldGenConstraints, TreeRequest}
import com.objectdynamics.tdg.schema.TestDataSchema
import com.objectdynamics.tdg.spec.{DataField, DataSetSpec}
import org.scalatest._

import scalaz.{-\/, \/-}

/**
  * Created by lee on 7/25/17.
  */
class BuilderSpec extends FlatSpec with Matchers {

  val constraints = Map("person" -> FieldGenConstraints("person", Set(BetweenSpec(100, 200))))
  val treeRequest=new TreeRequest( "person",10, constraints,None,None, false )
  val bldrReq = BuildRequest(treeRequest, 10)
  "A Builder" should "build 10 values" in {

    val fields = List( new DataField( "id", ScalaInt()))
    val dataSetSpecs = Map("person" -> DataSetSpec( "person", fields))
    val testDataSchema = new TestDataSchema("test", dataSetSpecs)
    new DefaultBuilder().build(bldrReq, testDataSchema) match {
      case -\/(err) => println(s"Error:  $err");fail()
      case \/-(testData) => testData.dataSetList.head.rows.size should be(10)
    }
  }

}
