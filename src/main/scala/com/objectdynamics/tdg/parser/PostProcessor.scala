package com.objectdynamics.tdg.parser

import com.objectdynamics.tdg.model.TestData
import com.objectdynamics.tdg.parser.model.{Assure, PostProcess}

/**
  * Created by lcollins on 8/24/2016.
  */
class PostProcessor {

  def process(testData: TestData, postProcess: PostProcess): TestData = {
    postProcess match {
      case assure:Assure => {
        process(testData, postProcess)
      }
      case _ => { throw new IllegalArgumentException("Unsupported postProcess")}
        
    }
    testData
  }

  def process(testData: TestData, assure: Assure): TestData = {

    testData
  }
}
