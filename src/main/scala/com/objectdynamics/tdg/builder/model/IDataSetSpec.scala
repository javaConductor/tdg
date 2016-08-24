package com.objectdynamics.tdg.builder.model

import com.objectdynamics.tdg.spec._

trait IDataSetSpec {

    def isNormal(fldName: String): Boolean;

    def hasAggregates: Boolean;

    //var name:String;
    def hasForeignRefs(): Boolean = !relationships.isEmpty;

    def isAggregate(fldName: String): Boolean;

    def isForeignRef(fldName: String): Boolean;

    def relationship(idx: Int): Option[Relationship];

    def relationships: List[Relationship];

    def relationshipsFor(fldName: String): List[Relationship];

    def fields: List[IDataField];

    def field(fldName: String): Option[IDataField];

    def fieldNames: List[String];
    var dataSetType: DataSetType;
    val name: String;
}
