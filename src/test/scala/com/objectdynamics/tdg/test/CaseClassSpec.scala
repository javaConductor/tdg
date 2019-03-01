package com.objectdynamics.tdg.test

import com.objectdynamics.tdg.builder.model.DataRow
import com.objectdynamics.tdg.builder.{BuildRequest, DefaultBuilder}
import com.objectdynamics.tdg.generators.ValueExtractor
import com.objectdynamics.tdg.parser.model._
import com.objectdynamics.tdg.schema.TestDataSchema
import com.objectdynamics.tdg.spec.datatypes.{DataType, ScalaDouble, ScalaInt, ScalaString}
import com.objectdynamics.tdg.spec.{DataField, DataSetSpec}
import org.scalatest._
import scalaz.{-\/, \/-}

/**
  * Created by lee on 7/25/17.
  */

case class Trade(id: Int, seller: String, buyer: String, amount: Int)

class CaseClassSpec extends FlatSpec with Matchers {

  implicit val valueExtractorStr: ValueExtractor[String] = ValueExtractor.StringExtractor
  implicit val valueExtractorInt: ValueExtractor[Int] = ValueExtractor.IntExtractor


  "A proper dss" should "be created" in {

    val sellers = List("Citadel", "Chase", "Citibank", "Wells Fargo", "Trader Jo", "BBC", "TCF", "Barclays", "Firm 47")
    val buyers = sellers.tail.reverse

    // Setup test data schema
    val dss = caseClassToDataSetSpec(classOf[Trade])

    var ok = dss.field("id") match {
      case Some(df: DataField) => {
        println("Id type: " + df.dataType)
        df.dataType == ScalaInt()
      }
      case _ => false
    }
    assert(ok, "id not int")

    ok = dss.field("seller") match {
      case Some(df: DataField) => {
        println("Seller type: " + df.dataType)
        df.dataType == ScalaString()
      }
      case _ => false
    }
    assert(ok, "seller not string")

    ok = dss.field("buyer") match {
      case Some(df: DataField) => {
        println("Buyer type: " + df.dataType)
        df.dataType == ScalaString()
      }
      case _ => false
    }
    assert(ok, "buyer not string")

    val dataSetSpecs = Map("trades" -> dss)
    val testDataSchema = TestDataSchema("test", dataSetSpecs)
  }


  "A Builder" should "build 100 trades" in {

    val sellers = List("Citadel", "Chase", "Citibank", "Wells Fargo", "Trader Jo", "BBC", "TCF", "Barclays")
    val buyers = sellers.reverse

    // Setup test data schema
    val dss = caseClassToDataSetSpec(classOf[Trade])
    val dataSetSpecs = Map("trades" -> dss)
    val testDataSchema = TestDataSchema("test", dataSetSpecs)

    /// create Build Request
    val constraints = Map(
      "id" -> FieldGenConstraints("id",  Set(BetweenSpec(1, 200))),
      //"id" -> FieldGenConstraints("id",  Set(EachSpec[Int](1 until 200 toList))),
      "seller" -> FieldGenConstraints("seller", Set(EachSpec(sellers))),
      "buyer" -> FieldGenConstraints("buyer", Set(EachSpec(buyers))),
      "amount" -> FieldGenConstraints("amount", Set(BetweenSpec(10000, 2000000))))
    val treeRequest = TreeRequest("trades", 100, constraints, None, None)
    val bldrReq = BuildRequest(treeRequest, 100)

    /// Build the test data
    new DefaultBuilder().build(bldrReq, testDataSchema) match {
      case -\/(err) => println(s"Error:  $err"); fail()
      case \/-(testData) => {
        testData.dataSetList.head.rows.size should be(100)
        println("")
        testData.dataSetList.head.rows.foreach(dr => {
          println("Data: " + dr.data)
          //          dr.data.map(pair => {
          //            val nm = pair._1
          //            val value = pair._2 // .as[Int]
          //            //assert(value > 10000 && value < 2000000)
          //            println("name: " + nm + " value: " + value)
          //
          //          })

        })
      }
    }
  }


  "A Builder" should "build 10 values" in {

    // Setup test data schema
    val fields = List(new DataField("id", ScalaInt()))
    case class Person(id: Int)
    val dss = caseClassToDataSetSpec(classOf[Person])
    val dataSetSpecs = Map("person" -> dss)
    val testDataSchema = TestDataSchema("test", dataSetSpecs)

    /// create Build Request
    val constraints = Map("id" -> FieldGenConstraints("id", Set(BetweenSpec(100, 200))))
    val treeRequest = TreeRequest("person", 10, constraints, None, None)
    val bldrReq = BuildRequest(treeRequest, 10)
    //println(s"TestDataSchema: $testDataSchema")
    //println(s"Person: ${testDataSchema.dssMap("person")}")

    /// Build the test data
    new DefaultBuilder().build(bldrReq, testDataSchema) match {
      case -\/(err) => println(s"Error:  $err"); fail()
      case \/-(testData) => {
        testData.dataSetList.head.rows.size should be(10)
        testData.dataSetList.head.rows.foreach(dr => {
          dr.data.map(pair => {
            val nm = pair._1
            val value = pair._2.as[Int]
            assert(value > 99 && value < 201)
            //println("name: " + nm + " value: " + value)

          })

        })
      }
    }
  }

  def checkValueBetween(row: DataRow, str: String, low: Int, hi: Int): Boolean = {
    val x = row.data(str).as[Int]
    x match {
      case n: Int => {
        //        println(s"$str: $n")
        n <= hi && n >= low
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
      case n: Int => {
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
      case n: String => {
        //          println(s"$fieldName: $n")
        values.contains(n)
      }
      case _ => {
        println(s"Error: $fieldName: $x")
        false
      }
    }

  }

  def caseClassToDataSetSpec[T](cls: Class[T]): DataSetSpec = {

    def ccToMap(cc: AnyRef) =
      (Map[String, Any]() /: cc.getClass.getDeclaredFields) {
        (a, f) =>
          f.setAccessible(true)
          a + (f.getName -> f.get(cc))
      }


    //cls.getDeclaredFields.foreach( println)
    val dfList = cls.getDeclaredFields
      .filterNot(f => f.getName == "$outer")
      .map(f => DataField(f.getName, classToDataType(f.getType), false, None))
      .toList
    //ccToMap()
    val dss: DataSetSpec = new DataSetSpec(cls.getName(), dfList)
    dss
  }

  def classToDataType(c: Class[_]): DataType = {

    if (c.equals(classOf[String]))
      ScalaString()
    else if (c.equals(classOf[Int]))
      ScalaInt()
    else if (c.equals(classOf[Double]))
      ScalaDouble()
    else ScalaString()
  }

}
