package com.objectdynamics.tdg.test

//import org.junit.Assert;

import com.objectdynamics.tdg.builder.model.IDataSet
import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner
import org.specs._
import com.objectdynamics.tdg.builder.BuildRunner;


@RunWith(classOf[JUnitSuiteRunner])
class ParserTest extends SpecificationWithJUnit
{

    
      "can create 20 withRows" in {
        val tstArray = Array("-ds", "RetirementAccounts",
          "-schema", "/home/lcollins/workspaces/tdg/svn/test-data-generator/trunk/src/main/resources/test2.tds.json",
          "-count", "20",
          "-schemaType", "json",
          "-outputType", "csv",
          "-outputFolder", "./data");
    
        val br = new BuildRunner(tstArray);
        val dsOpt = br.testData.dataSet("RetirementAccounts");
        dsOpt must beSome[IDataSet]
        val dsRows = dsOpt.get.rows;
        dsRows must haveSize(20)

          val dsEmpOpt = br.testData.dataSet("Employee");
           dsEmpOpt must beSome[IDataSet]
            val dsEmpRows = dsEmpOpt.get.rows;
           dsEmpRows must haveSize(7)
    
          val dsDepOpt = br.testData.dataSet("Department");
           dsEmpOpt must beSome[IDataSet]
            val dsDepRows = dsDepOpt.get.rows;
           dsDepRows must haveSize(7)

         }



      "can create 100 withRows with proper disbursement" in {
        val tstArray = Array("-ds", "RetirementAccounts",
          "-schema", "/home/lcollins/workspaces/tdg/svn/test-data-generator/trunk/src/main/resources/test2.tds.json",
          "-count", "100",
          "-schemaType", "json",
          "-outputType", "csv",
          "-outputFolder", "./data");

        val br = new BuildRunner(tstArray);
        val dsOpt = br.testData.dataSet("RetirementAccounts");
        dsOpt must beSome[IDataSet]
        val dsRows = dsOpt.get.rows;
        dsRows must haveSize(100)




         }



}
