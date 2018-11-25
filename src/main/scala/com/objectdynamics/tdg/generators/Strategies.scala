package com.objectdynamics.tdg.generators

/**
  * Created by lee on 11/25/18.
  */
sealed trait Strategy

object NoStrategy extends Strategy

object BetweenStrategy extends Strategy

object ListStrategy extends Strategy

object EachStrategy extends Strategy

object SameValueStrategy extends Strategy