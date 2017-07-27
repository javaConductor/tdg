package com.objectdynamics.tdg.schema

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import java.io.File
import java.math.BigDecimal

import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.parser._
import com.objectdynamics.tdg.parser.model.FieldGenConstraints
import com.objectdynamics.tdg.spec._
import com.objectdynamics.tdg.spec.datatypes._
import com.objectdynamics.tdg.util._

import scala.util.parsing.json._
;

class NativeJsonSchemaLoader extends MetadataLoader with LogContributor {


  def fromSource[A](source: A): ITestDataSchema = null

  def fromJson(jsonSchema: String): ITestDataSchema = {
    fromText(jsonSchema)
  }

  def fromText(text: String): ITestDataSchema = {
    createSchema(text)
  }

  def fromFile(fileName: String): ITestDataSchema = {
    val jsonSchema = FileHelper.fileAsString(new File(fileName))
    createSchema(jsonSchema)
  }

  private def createSchema(jsonSchema: String): ITestDataSchema = {
    var tds: ITestDataSchema = TestDataSchema("TDG");
    log("Creating schema - jsonSchema:" + jsonSchema);

    //Parse the given JSON string and return either a List[Any]if the JSON string specifies an Array,
    // or a  Map[String,Any] if the JSON string specifies an object.
    val xx = JSON.parseFull(jsonSchema);
    xx match {
      case Some(sch: List[Map[String, Any]]) =>
        //log("ParsedSome:" + sch.getClass);

        sch.foreach {
          ps: Map[String, Any] =>
            tds += (createDataSetSpec(ps))
        }
      case _ => {}
    }
    tds.dataSetSpecs foreach {
      ds => {
        log("createSchema: ds = " + ds._2)
      }
    }
    log("createSchema( ) = " + tds.toString);
    tds;
  }

  private def createDataSetSpec(jsonDss: Map[String, Any]): IDataSetSpec = {

    val name = jsonDss.get("name") match {
      case Some(nm: String) => nm;
      case _ => throw new SchemaParseExeption("DataSetSpec must have 'name'");

    }
    log("Creating DataSetSpec:" + name);

    var dss: IDataSetSpec = new DataSetSpec(name, List[IDataField]())
    dss.dataSetType = jsonDss.get("dataSetType") match {
      case Some("rdbms") => RDBMS;
      case Some("document") => DOCUMENT;
      case Some("json-store") => DOCUMENT;
      // case None => throw new SchemaParseExeption("Missing dataSetType.")  ;
      case _ => throw new SchemaParseExeption("Unknown dataSetType.");
    }

    val fields: List[IDataField] = jsonDss.get("fields") match {
      case Some(l: List[Any]) => createFields(dss.name, l, List[IDataField]());
      case _ => throw new SchemaParseExeption("No fields defined for DataSet '" + name + "'")
    }

    //    val tt = jsonDss.get("relationships");
    //    val relationships: List[Relationship] = jsonDss.get("relationships") match {
    //      case Some(l: List[Any]) => createRelationships(dss.name, l, List[Relationship]());
    //      case _ => List[Relationship]();
    //    }
    return new DataSetSpec(name, fields)
  }

  private def createRelationships(dssName: String, jsonFldList: List[Any], acc: List[Relationship]): List[Relationship] = {
    jsonFldList match {
      case Nil => log("createRelationships() = " + acc.toString); acc;
      case jsonFld :: otherFlds => {
        log("Creating relationships " + jsonFld.toString);
        val fldMap: Map[String, Any] = jsonFld.asInstanceOf[Map[String, Any]];

        val dataSetName: String = fldMap.get("dataSetName") match {
          case Some(nm: String) => nm;
          case _ => throw new SchemaParseExeption("Field must have dataSetName.")
        }

        val active: Boolean = fldMap.get("active") match {
          case Some(b) => TypeHelper.stringToBool(b.toString);
          case _ => false;
        }

        val disbursementSpec: Option[DisbursementSpec] = fldMap.get("disbursementSpec") match {
          case Some(n) =>
            Some(parseDisbursementSpec(n.asInstanceOf[Map[String, Any]]))

          case _ => None;
        }


        val fieldMap: Map[String, String] = fldMap.get("fieldMap") match {
          case Some(fmap: Map[String, Any]) => fmap map { (fromTo: Tuple2[String, Any]) => (fromTo._1, fromTo._2.toString) } toMap;
          case _ => throw new SchemaParseExeption("Relationship has no fields defined.")
        }

        val rel: Relationship = Relationship(dataSetName, fieldMap, Map[String, FieldGenConstraints](), disbursementSpec, active);

        log("Created Relationship to " + dataSetName);
        createRelationships(dssName, otherFlds, acc ::: List(rel));
      }
    }

  }


  private def createFields(dssName: String,
                           jsonFldList: List[Any],
                           acc: List[IDataField]): List[IDataField] = {

    jsonFldList match {
      case Nil => log("createFields() = " + acc.toString); acc;
      case jsonFld :: otherFlds => {
        log("Creating field " + jsonFld.toString);

        //                val fldMap: Map[String, Any] = ListHelper.mapFromTuplesList(jsonFld.asInstanceOf[List[Tuple2[String,
        //                        Any]]]);
        val fldMap: Map[String, Any] = jsonFld.asInstanceOf[Map[String, Any]];
        //log("createFields(): fldMap:" + fldMap)

        var df: IDataField = null;
        var dt: IDataTypeInstance = null;

        val fldName: String = fldMap.get("name") match {
          case Some(nm: String) => nm;
          case _ => throw new SchemaParseExeption("Field must have name.")
        }

        ////// deal with 'like' property
        fldMap.get("like") match {
          case Some(otherDs: String) =>
            acc find {
              _.name == otherDs
            } match {
              case None => throw new
                  SchemaParseExeption("DataField:" + fldName + "No such DataSet:" + otherDs)
              case Some(otherDf: IDataField) =>
                df = otherDf.asInstanceOf[DataField] makeLike fldName
                dt = df.dataType;

            }
          case _ =>
            //log("Creating field " + fldName + ":type:" + fldMap.get("type"));
            dt = DataTypeObject.fromOption(fldMap.get("type")
            match {
              case Some(x) => Some(x.toString)
              case _ => Some("text") // default type
            }
            )

            log("Creating field " + fldName + ": dataType:" + dt);
            df = new DataField(fldName, dt);
        }


        fldMap.get("generator") match {
          case Some(x: Any) => df.setGeneratorName((x.toString))
          case _ =>
        };

        var max = fldMap.get("max") match {
          case Some(x: Any) => Some(new scala.BigDecimal(new java.math.BigDecimal(x.toString)))
          case _ => None;
        };

        var min = fldMap.get("min") match {
          case Some(x: Any) => Some(new scala.BigDecimal(new java.math.BigDecimal(x.toString)))
          case _ => None
        };

        //                fldMap.get("foreignRef") match
        //                {
        //                    case Some(fref: Map[String, Any]) => df.foreignRef = parseForeignRef(fref);
        //                    case _ =>
        //                };

        var maxLength: Option[Int] = fldMap.get("maxLength") match {
          case Some(x: Any) => Some(new BigDecimal(x.toString).intValue())
          case _ => None;
        };
        var prefix: Option[String] = fldMap.get("prefix") match {
          case Some(x: Any) => Some(x.toString)
          case _ => None;
        };
        var pk: Boolean = fldMap.get("PK") match {
          case Some(b) => TypeHelper.stringToBool(b.toString);
          case _ => false
        };
        var suffix: Option[String] = fldMap.get("suffix") match {
          case Some(x: Any) => Some(x.toString)
          case _ => None;
        };
        var unique: Boolean = fldMap.get("unique") match {
          case Some(b) => TypeHelper.stringToBool(b.toString)
          case _ => false;

        };
        var data: Option[List[String]] = fldMap.get("data") match {
          case Some(d: List[Any]) => parseData(d);
          case _ => None;
        }
        val vv = data match {
          case Some(x: List[String]) => x.toString
          case _ => "No data."
        };

        log("Data: " + vv);
        var disbursementSpec: Option[DisbursementSpec] = fldMap.get("disbursementSpec") match {
          case Some(n) =>
            if (data == None) {
              throw new SchemaParseExeption("'data' is required where 'disbursementSpec'" +
                " is present. Dataset:" + dssName)
            };
            Some(parseDisbursementSpec(n.asInstanceOf[Map[String, Any]]))
          //                    case Some(x: Any) => df.dmin = parseDisbursementSpec()
          case _ => None;
        }

        log("Created field " + fldName + "[" + df.toString + "]:Recursing...");



        //                createFields(dssName, otherFlds, df :: acc);
        createFields(dssName,
          otherFlds,
          acc ::: List(DataField(fldName, dt, min, max, maxLength, prefix, suffix, unique, data, disbursementSpec)));
      }
    }

  }

  private def parseForeignRef(frefMap: Map[String, Any]): Option[ForeignRef] = {

    log("parsing ForeignRef:" + frefMap);

    val dsName: String = frefMap.get("dataSetName") match {
      case Some(n) => n.asInstanceOf[String]
      case _ => throw new SchemaParseExeption("dataSetName is required for ForeignRef")
    }
    val fldName: String = frefMap.get("fieldName") match {
      case Some(n) => n.asInstanceOf[String]
      case _ => throw new SchemaParseExeption("fieldName is required for ForeignRef")
    }
    val disbursementSpec: DisbursementSpec = parseDisbursementSpec(frefMap.get("disbursementSpec") match {
      case Some(n) => n.asInstanceOf[Map[String, Any]]
      case _ => throw new
          SchemaParseExeption("disbursementSpec is required for ForeignRef")
    });
    //val dataSetName: String, val fieldName: String, val disbursementSpec: DisbursementSpec
    Some(new ForeignRef(dsName, fldName, disbursementSpec));
  }

  private def parseDisbursementSpec(specMap: Map[String, Any]): DisbursementSpec = {
    //var percentages: List[Double] = List[Double]();
    log("parsing DisbursementSpec");

    specMap.get("ratio") match {
      case Some(r: List[Any]) => {
        log("parsing DisbursementSpec - ratio");

        new DisbursementSpec(new Tuple2[Double,
          Double](r.head.toString.toDouble,
          r.last.toString.toDouble));
      }
      case _ => {
        specMap.get("percentages") match {
          case Some(pcts: List[Any]) =>
            log("parsing DisbursementSpec - percentages");

            (new DisbursementSpec(convertAnyToDouble(pcts)));
          case _ => throw new ParsingException("Either percentages or ration is required.");
        }
      }
    }
  }

  private def convertAnyToDouble(pcts: List[Any]): List[Double] = {
    //"".asInstanceOf[String].toDouble
    pcts map { pct: Any => new BigDecimal(pct.toString).doubleValue }
  }

  private def parseData(data: List[Any]): Option[List[String]] = data match {
    case dl: List[Any] => val l: List[String] = dl map { x: Any => x.toString };
      Some(l);
  }

}