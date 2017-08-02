/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.objectdynamics.tdg

/// Generating data for a Database
/// tdg -build -schema inventory.tdg -dbInfo db.properties -count 25000 -outputType jdbc -outputFolder ./data

/// Generating data for a CSV file
/// tdg -build -schema inventory.tdg -count 26000 -outputType csv -outputFolder ./data

/// Generating data for a JSON file
/// tdg -build -schema inventory.tdg -count 26000 -outputType json -outputFolder ./data

// d:\data\generated\inventory

object MainApp //extends Application
{
  val executionStart: Long = 0L;
  var tstArray = Array("-ds", "RetirementAccounts",
    "-schema", "/home/lcollins/workspaces/tdg/svn/test-data-generator/trunk/src/main/resources/test3.tds.json",
    "-count", "2000",
    "-outputType", "csv",
    "-schemaType", "json",
    "-outputFolder", "./data");

  /**
    * @param args the command line arguments
    */
  def main(args: Array[String]) {

    //new BuildRunner(tstArray).toString;
  }
}
