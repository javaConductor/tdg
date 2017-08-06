package com.objectdynamics.tdg.test

import com.objectdynamics.tdg.builder.model.DataRow
import com.objectdynamics.tdg.builder.{BuildRequest, DefaultBuilder}
import com.objectdynamics.tdg.generators.valueFunction
import com.objectdynamics.tdg.parser.model.{BetweenSpec, FieldGenConstraints, InSpec, TreeRequest}
import com.objectdynamics.tdg.schema.TestDataSchema
import com.objectdynamics.tdg.spec.datatypes.ScalaInt
import com.objectdynamics.tdg.spec.{DataField, DataSetSpec}
import org.scalatest._

import scalaz.{-\/, \/-}

/**
  * Created by lee on 7/25/17.
  */
class RandomIntBuilderSpec extends FlatSpec with Matchers {

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

  "A Builder" should "build 50000 values between -1000 and 2000" in {
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

  "A Builder" should "build 50000 values between -10000 and -2000" in {
    val n = 50000
    val lo = -10000
    val hi = -2000
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


  "A Builder" should "build 50000 values in list 1,3,5,7,11,13,17,23,29,31,37,43" in {
    val n = 50000
    val lo = -10000
    val hi = -2000
    val nums = List(1,3,5,7,11,13,17,23,29,31,37,43)
    val constraints = Map("id" -> FieldGenConstraints("id", Set(InSpec(nums))))
    val treeRequest = TreeRequest( "person",n, constraints, None, None )
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
        rows.forall(  checkValueIn( _, "id", nums ) ) should be(true)
      }
    }
  }

  "A Builder" should "build 50 values in list 42" in {
    val n = 50
    val nums = List(42)
    val constraints = Map("id" -> FieldGenConstraints("id", Set(InSpec(nums))))
    val treeRequest = TreeRequest( "person",n, constraints, None, None )
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
        rows.forall(  checkValueIn( _, "id", nums ) ) should be(true)
      }
    }
  }

  "A Builder" should "build 50000 values between 100 and 200 and 500 to 600" in {
    val n = 50000
    val lo = 100
    val hi = 200
    val lo2 = 500
    val hi2 = 600
    val constraints = Map("id" -> FieldGenConstraints("id", Set(BetweenSpec(lo, hi))),
    "customerId" -> FieldGenConstraints("customerId", Set(BetweenSpec(lo2, hi2))))
    val treeRequest=TreeRequest( "person",n, constraints,None,None )
    val bldrReq = BuildRequest(treeRequest, n)
    val fields = List( new DataField( "id", ScalaInt()),new DataField( "customerId", ScalaInt()))
    val dataSetSpecs = Map("person" -> DataSetSpec( "person", fields))
    val testDataSchema = TestDataSchema("test", dataSetSpecs)
    println(s"TestDataSchema: $testDataSchema")
    println (s"Person: ${testDataSchema.dssMap("person")}" )
    new DefaultBuilder().build(bldrReq, testDataSchema) match {
      case -\/(err) => println(s"Error:  $err");fail()
      case \/-(testData) => {
        val rows = testData.dataSetList.head.rows
        rows.forall((dr) => {
          checkValueBetween( dr, "id", lo, hi ) &&
            checkValueBetween( dr, "customerId", lo2, hi2 )  }) should be(true)
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

  def checkValueIn(row: DataRow, str: String, nums: List[Int]): Boolean = {
    val x = new valueFunction(row.data(str) )()
    x match {
      case i:Option[Int] => {
        i match {
          case Some(n:Int) => {
            println(s"$str: $n")
            nums.contains(n)
          }
          case _ => {
            println(s"Error: $str: $x")
            false
          }
        }
      }
      case _ => {
        println(s"Error: $str: $x")
        false
      }
    }

  }

}
