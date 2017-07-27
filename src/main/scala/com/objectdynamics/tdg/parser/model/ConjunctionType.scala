package com.objectdynamics.tdg.parser.model

object ConjunctionType {
  def apply(s: String): ConjunctionType = {
    if (s == "or") OR else AND;
  }
}

class ConjunctionType {
}

object OR extends ConjunctionType;

object AND extends ConjunctionType;
