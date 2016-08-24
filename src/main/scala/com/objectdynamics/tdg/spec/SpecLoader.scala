package com.objectdynamics.tdg.spec

import java.io._

import com.objectdynamics.tdg.util.FileHelper

object SpecLoader
{
}

class SpecLoader(jsonSpec: String)
{


    def this(file: File) =
    {
        this (FileHelper.fileAsString(file));
    }

}