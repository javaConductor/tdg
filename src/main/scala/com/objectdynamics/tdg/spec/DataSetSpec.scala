package com.objectdynamics.tdg.spec

import com.objectdynamics.tdg.util._
import com.objectdynamics.tdg.builder.model._

object DataSetType
{};

trait DataSetType
{};

object RDBMS extends DataSetType;

object DOCUMENT extends DataSetType;


/**
 * This clas represents
 */
case class DataSetSpec(override val name: String,
                       fields: List[IDataField],
                       relationships: List[Relationship] = List.empty)
  extends IDataSetSpec
{
    var fieldMap: Map[String, IDataField] = createFieldMap(fields);
    var dataSetType: DataSetType = com.objectdynamics.tdg.spec.RDBMS;

    //var name:String;
    //def hasForeignRefs(): Boolean = !relationships.isEmpty

    //fields.foldLeft(false)((acc, df) => (if (acc) true else df.isForeignRef));

    def hasAggregates(): Boolean =
        fields.foldLeft(false)((acc, df) => (if(acc) true else isAggregate(df.name)))

    //    def isNormal(): Boolean =
    //        fields.foldLeft(true)((acc, df) => (if (!acc) false else df.isNormal))

    def isForeignRef(fldName: String): Boolean =
    {   if (relationships.isEmpty)
            false
        else
            isNormal(fldName) && !isAggregate(fldName);
    }

    def isAggregate(fldName: String): Boolean =
    {
        false
    }

    def isNormal(fldName: String): Boolean =
    {
        var ret: Boolean = true;
        relationships foreach
          {rel: Relationship => if(rel.fieldMap.keySet contains fldName) ret = false;}
        ret && !isAggregate(fldName);
    }

    def field(fieldName: String): Option[IDataField] = fieldMap.get(fieldName)

    def getField(fieldName: String): Option[IDataField] = field(fieldName)

    def aggregates: List[IDataField] = fields filter
      {dataFld: IDataField => isAggregate(dataFld.name)}

    def normalFields: List[IDataField] = fields filter
      {dataFld: IDataField => isNormal(dataFld.name)}

    def +(df: IDataField): IDataSetSpec =
        new DataSetSpec(name, df :: fields);

    /**
     *
     * @param fields :List[DataField]
     *
     * @return
     */
    def createFieldMap(fields: List[IDataField]): Map[String, IDataField] =
    {
        ListHelper.listToMap[String, IDataField](fields,
                                                 {f => f.name})
    };

    def fieldNames: List[String] = (fieldMap keys) toList;

    def relationship(idx: Int): Option[Relationship] =
    {

        if(relationships.isDefinedAt(idx))
        {
            Some(relationships(idx))
        }
        else
        {
            None
        }
    }

    def relationshipsFor(fldName: String): List[Relationship] =
    {

        relationships.filter
        {
            (r: Relationship) =>
                (!r.fieldConstraints.isEmpty) && r.fieldConstraints.keys.exists((s: String) => s.equals(fldName))
        }

    }


}

