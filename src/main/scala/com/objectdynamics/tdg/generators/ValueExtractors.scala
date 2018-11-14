package com.objectdynamics.tdg.generators

/**
  * Created by IntelliJ IDEA.
  * User: lcollins
  * Date: 11/9/18
  * Time: 1:19 AM
  */
sealed trait ValueExtractor[T] {
  def value(v: GeneratedValue): T
}

object ValueExtractor {

  implicit object IntExtractor extends ValueExtractor[Int] {
    def value(v: GeneratedValue): Int = {
      Integer.parseInt(v.value)
    }
  }

  implicit object StringExtractor extends ValueExtractor[String] {
    def value(v: GeneratedValue): String = {
      v.value
    }
  }
}