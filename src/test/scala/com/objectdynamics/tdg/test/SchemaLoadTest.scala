package com.objectdynamics.tdg.test

;

import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner
import org.specs._
import com.objectdynamics.tdg.schema._
import com.objectdynamics.tdg.builder.model._

@RunWith(classOf[JUnitSuiteRunner])
class SchemaLoaderTest extends SpecificationWithJUnit with SchemaLoadTestData
{
    var ctxt: Map[String, Any] = Map[String, Any]();

    def setCtxt(k: String, v: Any) =
    {
        ctxt += (k -> v);
    }

    def getCtxt(k: String): Option[Any] =
    {
        ctxt.get(k);
    }

    "can parse test schema" in
      {
          val schemaText = testSchema;
          val schemaLoader: NativeJsonSchemaLoader = new NativeJsonSchemaLoader();
          val schema: ITestDataSchema = schemaLoader.fromJson(schemaText);

          schema must be[ITestDataSchema];
          schema.dataSetSpec("Employee") must beSome[IDataSetSpec]
          schema.dataSetSpec("Department") must beSome[IDataSetSpec]
          schema.dataSetSpec("RetirementAccounts") must beSome[IDataSetSpec]
      }


    "can parse schema w like" in
      {
          val schemaText = schemaWithLike;
          val schemaLoader: NativeJsonSchemaLoader = new NativeJsonSchemaLoader();
          val schema: ITestDataSchema = schemaLoader.fromJson(schemaText);

          schema must be[ITestDataSchema];
          schema.dataSetSpec("Employee") must beSome[IDataSetSpec]
          val empSpec: IDataSetSpec = schema.dataSetSpec("Employee").get;

          empSpec.field("id") must beSome[IDataField]
          empSpec.field("deptId") must beSome[IDataField]
          empSpec.field("salary") must beSome[IDataField]
          empSpec.field("previousSalary") must beSome[IDataField]

          val salary: IDataField = empSpec.field("salary").get
          val previousSalary: IDataField = empSpec.field("previousSalary").get

          previousSalary.dataType must beEqualTo(salary.dataType);
          previousSalary.disbursementSpec must beEqualTo(salary.disbursementSpec);
          previousSalary.unique must beEqualTo(salary.unique);

          val tool1: IDataField = empSpec.field("tool1").get;
          val tool2: IDataField = empSpec.field("tool2").get;

          tool2.dataType must beEqualTo(tool1.dataType);

      }

}
