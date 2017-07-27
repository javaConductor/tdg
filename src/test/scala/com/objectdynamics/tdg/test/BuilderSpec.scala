package com.objectdynamics.tdg.test

import org.scalatest._

/**
  * Created by lee on 7/25/17.
  */
class BuilderSpec extends FlatSpec with Matchers {

  "A Stack" should "pop values in last-in-first-out order" in {

    new
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be(2)
    stack.pop() should be(1)
  }

}
