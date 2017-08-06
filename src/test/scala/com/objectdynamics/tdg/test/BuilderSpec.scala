package com.objectdynamics.tdg.test

import com.objectdynamics.tdg.builder.model.DataRow
import com.objectdynamics.tdg.builder.{BuildRequest, DefaultBuilder}
import com.objectdynamics.tdg.generators.valueFunction
import com.objectdynamics.tdg.parser.model.{BetweenSpec, FieldGenConstraints, TreeRequest}
import com.objectdynamics.tdg.schema.TestDataSchema
import com.objectdynamics.tdg.spec.datatypes.ScalaInt
import com.objectdynamics.tdg.spec.{DataField, DataSetSpec}
import org.scalatest._

import scalaz.{-\/, \/-}

/**
  * Created by lee on 7/25/17.
  */
class BuilderSpec extends FlatSpec with Matchers {

  "A Builder" should "build 10 values" in {

    val constraints = Map("person" -> FieldGenConstraints("person", Set(BetweenSpec(100, 200))))
    val treeRequest=TreeRequest( "person",10, constraints,None,None )
    val bldrReq = BuildRequest(treeRequest, 10)
    val fields = List( new DataField( "id", ScalaInt()))
    val dataSetSpecs = Map("person" -> DataSetSpec( "person", fields))
    val testDataSchema = TestDataSchema("test", dataSetSpecs)
    println(s"TestDataSchema: $testDataSchema")
    println (s"Person: ${testDataSchema.dssMap("person")}" )
    new DefaultBuilder().build(bldrReq, testDataSchema) match {
      case -\/(err) => println(s"Error:  $err");fail()
      case \/-(testData) => {
        testData.dataSetList.head.rows.size should be(10)
      }
    }
  }

  def checkValueBetween(row: DataRow, str: String, low: Int, hi: Int): Boolean = {
    val x = new valueFunction(row.data("id") )()
    x match {
      case i:Option[Int] => {
        i match {
          case Some(n:Int) => {
            println(s"n: $n")
            n  <= hi && n  >= low
          }
          case _ => false
        }
      }
      case _ => false
    }
  }

  "A Builder" should "build 10 values between 100 and 200" in {
    val n = 50000
    val lo = -1000
    val hi = 2000
    val constraints = Map("id" -> FieldGenConstraints("id", Set(BetweenSpec(lo, hi))))
    val treeRequest=TreeRequest( "person",n, constraints,None,None )
    val bldrReq = BuildRequest(treeRequest, n)
    val fields = List( new DataField( "id", ScalaInt()))
    val dataSetSpecs = Map("person" -> DataSetSpec( "person", fields))
    val testDataSchema = TestDataSchema("test", dataSetSpecs)
    println(s"TestDataSchema: $testDataSchema")
    println (s"Person: ${testDataSchema.dssMap("person")}" )
    new DefaultBuilder().build(bldrReq, testDataSchema) match {
      case -\/(err) => println(s"Error:  $err");fail()
      case \/-(testData) => {
        val rows = testData.dataSetList.head.rows
        rows.forall(  checkValueBetween( _, "id", lo, hi ) ) should be(true)
      }
    }
  }

}
