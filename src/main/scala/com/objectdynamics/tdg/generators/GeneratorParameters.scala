package com.objectdynamics.tdg.generators

case class GeneratorParameters(min: Option[Any],
                               max: Option[Any],
                               particularValue: Option[Any],
                               data: List[Option[Any]],
                               valueExclusions: List[Any]) {

}

object DefaultGenParameters extends GeneratorParameters(None, None, None, Nil, Nil);
