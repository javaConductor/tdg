package com.objectdynamics.tdg.model

import com.objectdynamics.tdg.util._
import java.lang.String
import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.generators.NamedGeneratedValue
import com.objectdynamics.tdg.builder.ValueFunction

/**
 * This class represents a row in RDBMS Table or Key/Value pair in a No-SQL Collection
 *
 * C
 *    D <: IDataTypeObject,
    E <: IDataTypeInstance,
    B <: IDataSetSpec[IDataField[D,E]]

 */
case class DataRow(dataSetSpec: IDataSetSpec, objectId: String, published: Boolean, data: Map[String, ValueFunction] = Map.empty)
  extends IDataRow with LogContributor
{

    def this(dataSetSpec: IDataSetSpec, objectId: String) = this (dataSetSpec, objectId, false);

    //var data:Map[String, Option[Any] ]
    def fields(): List[String] = dss.fields map {
          _.name
      };

    //var published: Boolean = false;


    def +(fldValue: NamedGeneratedValue): IDataRow = null

    def id: String = objectId;

    def asPublished = this.copy(published = true);


    var isComplete: Boolean =
    {
        val b = hasFields(ListHelper.listToSet(fields))
        // log(this + ": complete=" + b);
        b

    };

    def dss: IDataSetSpec =
    {
        dataSetSpec
    };

    def complete: Boolean = isComplete

    //val dataField:DataField;
    //    override def value(field: String): Option[Any] = data.get(field) match
    //    {
    //        case Some(opt: Option[Any]) => opt
    //        case None => None
    //    };

    def hasFields(flds: Set[String]): Boolean =
    {

        flds.forall
          {
              data.get(_) match
              {
                  case Some(x) => true;
                  case None => false;
              }
          }
    };

    def definesFields(flds: Set[String]): Boolean = flds.size == (fields intersect flds.toList).length;

    def +(dataItem: Tuple2[String, ValueFunction]) =
    {

        this.copy(dataSetSpec = dss, data = this.data + (dataItem._1 -> dataItem._2));
    }

    //def +(fldValue: NamedGeneratedValue):DataRow = this + ( fldValue.name -> valueFunction(fldValue) );

    def fieldSet(flds: Set[String]): Map[String, ValueFunction] =
    {
        (flds map
          {fldname: String => (fldname -> value(fldname))}) toMap;
    }

    def withFields(flds: Set[String]): DataRow = this.copy(dataSetSpec = dss, data = fieldSet(flds));

    def withDataRenamedTo(fmap: Map[String, String]): DataRow =
    {
        val nuData: Map[String, ValueFunction] = fmap map
          {
              flds: Tuple2[String, String] =>
                  val currentFld = flds._1;
                  val nuFld = flds._2;
                  (nuFld -> value(currentFld))
          }
        this.copy(data = nuData);
    }

    override def toString: String = "DataRow(" + dss.name + ")" + (if (this.published) " PUBLISHED->" else "."  )+ id ;
}

//class PartialRow(override val dss: DataSetSpec, override val data: Map[String, ValueFunction])
//  extends DataRow(dss:
//        DataSetSpec,
//  data: Map[String, ValueFunction])
//  {
//  //  override def complete = false;
//
//  //new  Map[String, Option[Any] ]()
//  override def withFields(flds: Set[String]): DataRow = {
//    new PartialRow(dss, fieldSet(flds));
//  }
//
//  }

//PartialRow
