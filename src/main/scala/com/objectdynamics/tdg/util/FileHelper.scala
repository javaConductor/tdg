package com.objectdynamics.tdg.util

import java.io.{BufferedReader, File}

/**
  * Created by IntelliJ IDEA.
  * User: Lee
  * Date: Nov 26, 2010
  * Time: 2:39:19 AM
  * To change this template use File | Settings | File Templates.
  */

object FileHelper {

  def fileAsString(file: File): String = ""//FileUtils.readFileToString(file)

  def readLines(b: BufferedReader, linesSoFar: String): String = {
    b.ready match {
      case true => readLines(b, linesSoFar + b.readLine + "\n");
      case false => linesSoFar;
    }
  }
}