package com.objectdynamics.tdg.test

import java.io.File

import com.objectdynamics.tdg.builder.model.DataRow
import com.objectdynamics.tdg.builder.{BuildRequest, DefaultBuilder}
import com.objectdynamics.tdg.generators.ValueExtractor
import com.objectdynamics.tdg.parser.model._
import com.objectdynamics.tdg.schema.TestDataSchema
import com.objectdynamics.tdg.spec.datatypes.{ScalaFloat, ScalaInt, ScalaString}
import com.objectdynamics.tdg.spec.{DataField, DataSetSpec}
import com.objectdynamics.tdg.targets.{CSVFileDataTarget, WriteManifest}
import org.scalatest._

import scalaz.{-\/, \/-}

/**
  * Created by lee on 7/25/17.
  */
class CSVBuilderSpec extends FlatSpec with Matchers {

  implicit val valueExtractorStr: ValueExtractor[String] = ValueExtractor.StringExtractor
  implicit val valueExtractorInt: ValueExtractor[Int] = ValueExtractor.IntExtractor

  "A Builder" should "build 50000 and save them to a csv file" in {
    val n = 50000
    val lo = 20
    val hi = 80
    val outputFolder = "/home/lee/git/tdg"
    val DATA_SET_NAME = "person"
    val names = List("Eve", "Adam", "Ragnar", "Lee", "David",
      "Henry", "Cassandra", "Steve", "Prince Planet", "Ruby",
      "Tony", "Terry", "Arthur", "Galen", "Cindy", "Josiah")
    val constraints = Map(
      "name" -> FieldGenConstraints("name", Set(InSpec(names))),
      "age" -> FieldGenConstraints("age", Set(BetweenSpec(lo, hi)))
    )
    val treeRequest = TreeRequest(DATA_SET_NAME, n, constraints, None, None)
    val bldrReq = BuildRequest(treeRequest, n)
    val fields = List(
      new DataField("name", ScalaString()),
      new DataField("age", ScalaInt())
    )
    val dataSetSpecs = Map(DATA_SET_NAME -> DataSetSpec(DATA_SET_NAME, fields))
    val testDataSchema = TestDataSchema("test", dataSetSpecs)
    new DefaultBuilder().build(bldrReq, testDataSchema) match {
      case -\/(err) => println(s"Error:  $err"); fail()
      case \/-(testData) => {
        val rows: List[DataRow] = testData.dataSetList.head.rows
        rows.forall((dr) => {
          checkValueInStr(dr, "name", names) &&
            checkValueBetween(dr, "age", lo, hi)
        }) should be(true)

        val target: CSVFileDataTarget = new CSVFileDataTarget(outputFolder)
        val wm = target.store(testData)
        checkCsvFile(wm, outputFolder, DATA_SET_NAME , 50000)
        val outputFilename = outputFolder + "/" + DATA_SET_NAME + ".csv"
        new File(outputFilename).exists() should be(true)
        new File(outputFilename).delete()
      }
    }
  }

  "A Builder" should "build 50000 person and 1000 products and save them to 2 csv files" in {
    val n = 50000
    val lo = 20
    val hi = 80
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

    // constraints
    val constraints = Map(
      "name" -> FieldGenConstraints("name", Set(InSpec(names))),
      "age" -> FieldGenConstraints("age", Set(BetweenSpec(lo, hi)))
    )

    val prodConstraints = Map(
      "name" -> FieldGenConstraints("name", Set(EachSpec(products))),
      "price" -> FieldGenConstraints("price", Set(EachSpec(productPrices)))
    )

    // builder request
    val treeRequest2 = TreeRequest(DATA_SET_NAME_2, n, prodConstraints, None, None)
    val treeRequest = TreeRequest(DATA_SET_NAME, n, constraints, None,None)
    val bldrReq = BuildRequest(treeRequest, n,  List(treeRequest2))

    // build it !!
    new DefaultBuilder().build(bldrReq, testDataSchema) match {
      case -\/(err) => println(s"Error:  $err"); fail()
      case \/-(testData) => {
        val rows: List[DataRow] = testData.dataSetList.head.rows
        rows.forall((dr) => {
          checkValueInStr(dr, "name", names) &&
            checkValueBetween(dr, "age", lo, hi)
        }) should be(true)

        val target: CSVFileDataTarget = new CSVFileDataTarget(outputFolder)
        val wm = target.store(testData)
        checkCsvFile(wm, outputFolder, DATA_SET_NAME, n)
        checkCsvFile(wm, outputFolder, DATA_SET_NAME_2, n)

        new File(outputFolder, DATA_SET_NAME+".csv").delete()
        new File(outputFolder, DATA_SET_NAME_2+".csv").delete()
      }
    }
  }

  def checkCsvFile(wm:WriteManifest, folderName:String, dsName:String , rowCount:Int) = {
    val outputFilename = folderName + "/" + dsName + ".csv"
    // find ds
    wm.dataSetList.find(dsm => dsm.outputFileName == outputFilename) match {
      case Some(dsm) => {
       dsm.numRows should be(rowCount) }
    }
    new File(outputFilename).exists() should be(true)

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
}
