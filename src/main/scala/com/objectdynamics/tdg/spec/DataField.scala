package com.objectdynamics.tdg.spec

import java.lang.String
import com.objectdynamics.tdg.spec.datatypes._

//import com.objectdynamics.tdg.builder.model

import com.objectdynamics.tdg.builder.model._

case class DataField(name: String,
                     dataType: IDataTypeInstance,
                     minIn: Option[BigDecimal],
                     maxIn: Option[BigDecimal],
                     maxLengthIn: Option[Int],
                     prefixIn: Option[String],
                     suffixIn: Option[String],
                     uniqueIn: Boolean,
                     dataIn: Option[List[String]],
                     disbursementSpecIn: Option[DisbursementSpec]
                      ) extends IDataField
{
    def this(name: String, dataType: IDataTypeInstance) =
    {

        this (name, dataType, None, None, None, None, None, false, None, None);
    }

    override def min: Option[BigDecimal] = minIn

    override def max: Option[BigDecimal] = maxIn

    override def maxLength: Option[Int] = maxLengthIn

    override def prefix: Option[String] = prefixIn

    override def suffix: Option[String] = suffixIn

    override def unique: Boolean = uniqueIn

    override def data: Option[List[String]] = dataIn

    override def disbursementSpec: Option[DisbursementSpec] = disbursementSpecIn

    override def generatorName: Option[String] = generator;

    var primaryKey: Boolean = false;
    val fieldType: FieldType = ASingleValue;
    var generator: Option[String] = None;

    def isAggregate: Boolean =
    {
        false; //foreignRef != None;
    }

    @deprecated
    def makeLike(name: String): DataField =
    {
        var df: DataField = DataField(name, dataType, min, max, maxLength, prefix, suffix, unique, data, disbursementSpec);
        df
    }

    override def toString: String = "DataField(" + name + "," + dataType + ")"

    //    def min_=(m: Option[BigDecimal])
    //    {}
    //
    //    def unique_=(b: Boolean) = null
    //
    //    def max_=(m: Option[BigDecimal])
    //    {}
    //
    //    def suffix_=(s: Option[String])
    //    {}

    //    def prefix_=(s: Option[String])
    //    {}

    def withMin(m: Option[BigDecimal]): DataField =
    {
        this.copy(minIn = m);
    }

    def withMax(m: Option[BigDecimal]): DataField =
    {
        this.copy(maxIn = m);
    }

    def withUnique(u: Boolean): DataField =
    {
        this.copy(uniqueIn = u);
    }

    def withSuffix(m: Option[String]): DataField =
    {
        this.copy(suffixIn = m);
    }

    def withPrefix(px: Option[String]): DataField =
    {
        this.copy(prefixIn = px);
    }

    def setGeneratorName(n: String)
    {
        this.generator = Some(n);
    }
}