package com.objectdynamics.tdg.test.parser

import java.io.File

import com.objectdynamics.tdg.builder.model.DataRow
import com.objectdynamics.tdg.builder.{BuildRequest, DefaultBuilder}
import com.objectdynamics.tdg.generators.ValueExtractor
import com.objectdynamics.tdg.parser.RequestParser
import com.objectdynamics.tdg.schema.TestDataSchema
import com.objectdynamics.tdg.spec.datatypes.{ScalaFloat, ScalaInt, ScalaString}
import com.objectdynamics.tdg.spec.{DataField, DataSetSpec}
import com.objectdynamics.tdg.targets.{CSVFileDataTarget, WriteManifest}
import org.scalatest._

import scalaz.{-\/, \/-}

/**
  * Created by lee on 7/25/17.
  */
class ParserSpec extends FlatSpec with Matchers {

  implicit val valueExtractorStr: ValueExtractor[String] = ValueExtractor.StringExtractor
  implicit val valueExtractorInt: ValueExtractor[Int] = ValueExtractor.IntExtractor
  /*
   request 100 Customer including ()
   specific(
       Customer(7) where age > 21 with
   (Purchase(12) where date > 1999 / 12 / 31 with
   PurchaseItem(11) where price > 20.25 and qty = 1 and
           Purchase(1) where date < 1999 / 12 / 31 with
   PurchaseItem(1) where price > 25.75 and qty = 1
   ) and
   Customer(7) where age < 16 with
   (Purchase(3) where date > 1999 / 12 / 31 with
   PurchaseItem(75) where price > 20.25 and qty = 1 and
           Purchase(5) where date < 1999 / 12 / 31 with
   PurchaseItem(1) where price > 25.75 and qty = 1
   )

   )*/

  "Request Parser" should " parse request" in {
    val optReq: Option[BuildRequest] = RequestParser.parseRequest(
      """ request Customer(100) where age between 20, 80 and name in ("lee", "david")"""
    )

//    optReq should beSome[BuildRequest]
    val req:BuildRequest = optReq.get
    System.out.println(req.toString)
    req.rootRequest.rows shouldBe 100
//    assert ( req.nRows == 100 )
  }

  "Request Parser" should " parse request and create 100 rows of each" in {
    val hi=100
    val lo=20
    val n=100
    val outputFolder = "/home/lee/git/tdg"
    val DATA_SET_NAME = "Person"
    val DATA_SET_NAME_2 = "Product"

    val names = List("Eve", "Adam", "Ragnar", "Lee", "David",
      "Henry", "Cassandra", "Steve", "Prince Planet", "Ruby",
      "Tony", "Terry", "Arthur", "Galen", "Cindy", "Josiah")

    val products = List("Marble Head", "Bread", "Carrots", "Oreo Cookies")
    val productPrices = List(9.99, 1.99, 0.50, 1.25)

    // schema
    val personFields = List(
      new DataField("name", ScalaString()),
      new DataField("age", ScalaInt())
    )
    val productFields = List(
      new DataField("name", ScalaString()),
      new DataField("price", ScalaFloat())
    )
    val dataSetSpecs = Map(
      DATA_SET_NAME_2 -> DataSetSpec(DATA_SET_NAME_2, productFields),
      DATA_SET_NAME -> DataSetSpec(DATA_SET_NAME, personFields))

    val testDataSchema = TestDataSchema("test", dataSetSpecs)

    val optReq: Option[BuildRequest] = RequestParser.parseRequest(
      s"""request $DATA_SET_NAME($n) where age between $lo, $hi and name in ("Lee", "David")"""
    )

    val req = optReq.get

    System.out.println(req.toString)
    //req.nRows shouldBe n
    req.rootRequest.rows shouldBe n

    new DefaultBuilder().build(req, testDataSchema) match {
      case -\/(err) => println(s"Error:  $err"); fail()
      case \/-(testData) => {
        val rows: List[DataRow] = testData.dataSetList.head.rows
//        rows.map((dr) => {
//          checkValueInStr(dr, "name", names) &&
//          checkValueBetween(dr, "age", lo, hi)
//        }).foreach(println(_))
        rows.forall((dr) => {
          checkValueInStr(dr, "name", names) &&
            checkValueBetween(dr, "age", lo, hi)
        }) should be(true)

        val target: CSVFileDataTarget = new CSVFileDataTarget(outputFolder)
        val wm = target.store(testData)
        checkCsvFile(wm, outputFolder, DATA_SET_NAME, n)
//        checkCsvFile(wm, outputFolder, DATA_SET_NAME_2, n)

        new File(outputFolder, DATA_SET_NAME+".csv").delete()
//        new File(outputFolder, DATA_SET_NAME_2+".csv").delete()
      }
    }
  }


  def checkCsvFile(wm:WriteManifest, folderName:String, dsName:String , rowCount:Int) = {
    val outputFilename = folderName + "/" + dsName + ".csv"
    // find ds
    wm.dataSetList.find(dsm => dsm.outputFileName == outputFilename) match {
      case Some(dsmObj) =>  dsmObj.numRows should be(rowCount)
      case _ => fail(s"Output file [$outputFilename] not in manifest.")
    }
    new File(outputFilename).exists() should be(true)
  }

  def checkValueBetween(row: DataRow, str: String, low: Int, hi: Int): Boolean = {
    val x = row.data(str).as[Int]
    x match {
      case n: Int => {
       //         println(s"$str: $n")
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
           // println(s"$fieldName: $n")
            values.contains(n)
          }
          case _ => {
            println(s"Error: $fieldName: $x")
            false
          }
    }

  }

}
