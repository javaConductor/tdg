package com.objectdynamics.tdg.test

import com.objectdynamics.tdg.builder.model.DataRow
import com.objectdynamics.tdg.builder.{BuildRequest, DefaultBuilder}
import com.objectdynamics.tdg.generators.valueFunction
import com.objectdynamics.tdg.parser.model.{BetweenSpec, FieldGenConstraints, InSpec, TreeRequest}
import com.objectdynamics.tdg.schema.TestDataSchema
import com.objectdynamics.tdg.spec.datatypes.{ScalaInt, ScalaString}
import com.objectdynamics.tdg.spec.{DataField, DataSetSpec}
import org.scalatest._

import scalaz.{-\/, \/-}

/**
  * Created by lee on 7/25/17.
  */
class RandomStringBuilderSpec extends FlatSpec with Matchers {


  "A Builder" should "build 50000 values with name from list and age between 20 and 80" in {
    val n = 50000
    val lo = 20
    val hi = 80
    val names = List("Lee","David", "Henry", "Cassandra", "Steve", "Prince Planet", "Ruby", "Tony")
    val constraints = Map(
      "name" -> FieldGenConstraints("name", Set(InSpec(names))),
      "age" -> FieldGenConstraints("age", Set(BetweenSpec(lo, hi)))
    )
    val treeRequest=TreeRequest( "person",n, constraints,None,None )
    val bldrReq = BuildRequest(treeRequest, n)
    val fields = List(
      new DataField( "name", ScalaString()),
      new DataField( "age", ScalaInt())
    )
    val dataSetSpecs = Map("person" -> DataSetSpec( "person", fields))
    val testDataSchema = TestDataSchema("test", dataSetSpecs)
    println(s"TestDataSchema: $testDataSchema")
    println (s"Person: ${testDataSchema.dssMap("person")}" )
    new DefaultBuilder().build(bldrReq, testDataSchema) match {
      case -\/(err) => println(s"Error:  $err");fail()
      case \/-(testData) => {
        val rows: List[DataRow] = testData.dataSetList.head.rows
        rows.forall((dr) => {
          checkValueIn(dr, "name", names) &&
            checkValueBetween(dr, "age", lo, hi)
        }) should be(true)
      }
    }
  }



  def checkValueBetween(row: DataRow, str: String, low: Int, hi: Int): Boolean = {
    val x = new valueFunction(row.data(str) )()
    x match {
      case Some(n:Int) => {
        println(s"$str: $n")
        n  <= hi && n  >= low
      }
      case _ => {
        println(s"Error: $str: $x")
        false
      }
    }
  }


  def checkValueIn[T](row: DataRow, fieldName: String, values: List[T]): Boolean = {
    val x = new valueFunction(row.data(fieldName) )()
    x match {
          case Some(n:T) => {
            println(s"$fieldName: $n")
            values.contains(n)
          }
          case _ => {
            println(s"Error: $fieldName: $x")
            false
          }
      case _ => {
        println(s"Error: $fieldName: $x")
        false
      }
    }

  }

}
