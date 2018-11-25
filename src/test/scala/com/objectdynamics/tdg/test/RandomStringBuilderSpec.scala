package com.objectdynamics.tdg.test

import com.objectdynamics.tdg.builder.model.DataRow
import com.objectdynamics.tdg.builder.{BuildRequest, DefaultBuilder}
import com.objectdynamics.tdg.generators.ValueExtractor
import com.objectdynamics.tdg.parser.model.{BetweenSpec, FieldGenConstraints, InSpec, TreeRequest}
import com.objectdynamics.tdg.schema.TestDataSchema
import com.objectdynamics.tdg.spec.datatypes.{ScalaInt, ScalaString}
import com.objectdynamics.tdg.spec.{DataField, DataSetSpec}
import org.scalatest._

import scala.collection.mutable.ListBuffer
import scalaz.{-\/, \/-}

/**
  * Created by lee on 7/25/17.
  */
class RandomStringBuilderSpec extends FlatSpec with Matchers {

  implicit val valueExtractorStr: ValueExtractor[String] = ValueExtractor.StringExtractor
  implicit val valueExtractorInt: ValueExtractor[Int] = ValueExtractor.IntExtractor

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
   // println(s"TestDataSchema: $testDataSchema")
   // println (s"Person: ${testDataSchema.dssMap("person")}" )
    new DefaultBuilder().build(bldrReq, testDataSchema) match {
      case -\/(err) => println(s"Error:  $err");fail()
      case \/-(testData) => {
        val rows: List[DataRow] = testData.dataSetList.head.rows
        rows.forall((dr) => {
          checkValueInStr(dr, "name", names) &&
            checkValueBetween(dr, "age", lo, hi)
        }) should be(true)
      }
    }
  }


  "A Builder" should "build 50000 and create case classes the hard way" in {
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
    //println(s"TestDataSchema: $testDataSchema")
    //println (s"Person: ${testDataSchema.dssMap("person")}" )
    new DefaultBuilder().build(bldrReq, testDataSchema) match {
      case -\/(err) => println(s"Error:  $err");fail()
      case \/-(testData) => {
        val rows: List[DataRow] = testData.dataSetList.head.rows
        rows.forall((dr) => {
          checkValueInStr(dr, "name", names) &&
            checkValueBetween(dr, "age", lo, hi)
        }) should be(true)

        case class Person(name:String, age:Int)
        val ll = new ListBuffer[Person]
        rows.foreach((dr) => {
          val name = dr.data("name").as[String]
          val age = dr.data("age").as[Int]
            ll.append(Person (
              name.asInstanceOf[String],
              age.asInstanceOf[Int]))
        })
      }
    }
  }

  def checkValueBetween(row: DataRow, str: String, low: Int, hi: Int): Boolean = {
    val x = row.data(str).as[Int]
    x match {
      case n:Int => {
//        println(s"$str: $n")
        n  <= hi && n  >= low
      }
      case _ => {
        println(s"Error: $str: $x")
        false
      }
    }
  }


  def checkValueIn(row: DataRow, fieldName: String, values: List[Int]): Boolean = {
    val x = row.data(fieldName).as[Int](valueExtractorInt)
    x match {
          case n:Int => {
  //          println(s"$fieldName: $n")
            values.contains(n)
          }
          case _ => {
            println(s"Error: $fieldName: $x")
            false
          }
    }

  }

  def checkValueInStr(row: DataRow, fieldName: String, values: List[String]): Boolean = {
    val x = row.data(fieldName).as[String]
    x match {
          case n:String => {
  //          println(s"$fieldName: $n")
            values.contains(n)
          }
          case _ => {
            println(s"Error: $fieldName: $x")
            false
          }
    }

  }

}
