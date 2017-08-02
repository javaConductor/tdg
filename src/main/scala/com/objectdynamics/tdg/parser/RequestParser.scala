package com.objectdynamics.tdg.parser

import com.objectdynamics.tdg.builder._
import com.objectdynamics.tdg.parser.model._

import scala.util.parsing.combinator.JavaTokenParsers

/*
*
   request 100 Customer including ()
   specific (
       Customer(7) where age > 21 with
       (Purchase(12) unique where date > 1999/12/31 with
           PurchaseItem(11) where price > 20.25 and qty = 1 and
       Purchase(1) unique where date < 1999/12/31 with
           PurchaseItem(1) unique where price > 25.75 and qty = 1
       )  and
       Customer(7) where age < 16 with
       (Purchase(3)  unique where date > 1999/12/31 with
           PurchaseItem(75) where price > 20.25 and qty = 1 and
       Purchase(5) unique where date < 1999/12/31 with
           PurchaseItem(1) where price > 25.75 and qty = 1
       )

   )
*/

object RequestParser extends JavaTokenParsers {

  def parseRequest(s: String): Option[BuildRequest] = {

    val pr: ParseResult[BuildRequest] = parseAll(request, s);
    if (pr.successful) {
      scala.Some(pr.get)
    }
    else {
      System.out.println("Not parsed: parseRequest(" + s + ")=" + pr.toString);
      scala.None
    }

  }

  def number = decimalNumber | wholeNumber | floatingPointNumber;

  def literalValue: Parser[String] = {
    (stringLiteral | number | dateValue) ^^ { case x => x.toString }
  }

  def fieldValue: Parser[String] = ident

  def dateValue: Parser[String] = "(19|20)\\d\\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])".r

  def value: Parser[String] = literalValue;

  def valueList: Parser[List[String]] = "(" ~ repsep(value, ",") ~ ")" ^^ {
    case "(" ~ lst ~ ")" => (lst);
  };

  def eqSpec: Parser[FieldGenConstraint] = "eq" ~> value ^^ {
    case vList => EqSpec(vList)
  }


  def inSpec: Parser[FieldGenConstraint] = "in" ~> valueList ^^ {
    case vList => InSpec(vList)
  }

  def betweenSpec: Parser[FieldGenConstraint] = betweenDtSpec | betweenNumberSpec;

  def betweenNumberSpec: Parser[FieldGenConstraint] = ("between" ~> number ~ "," ~ number) ^^ {
    case min ~ cma ~ max => BetweenSpec(min.toInt, max.toInt)
  }

  def betweenDtSpec: Parser[FieldGenConstraint] = ("between" ~> dateValue ~ "," ~ dateValue) ^^ {
    case min ~ cma ~ max => BetweenSpec(min.toInt, max.toInt)
  }

  def fldGenConstraint: Parser[FieldGenConstraint] = (betweenSpec | inSpec | eqSpec)

  def fldGenConstraintList: Parser[List[FieldGenConstraint]] = repsep(fldGenConstraint, "+")

  def fldSpec: Parser[FieldGenConstraints] = fieldValue ~ fldGenConstraintList ^^ {
    case fldName ~ segList => FieldGenConstraints(fldName, segList.toSet);
  }

  def fldGenSpecList: Parser[Map[String, FieldGenConstraints]] = repsep(fldSpec, "and") ^^ {
    case speLst => speLst map { fgs: FieldGenConstraints => (fgs.fieldName -> fgs) } toMap
  }

  // def fldGenSpecList = fieldValue ~ fldGenConstraintList
  /*
  *
 class BuildRequest(val rootObject: String, val nRows: Int, val _specifics: List[TreeRequest]= List.empty)
 {
     def specifics: List[TreeRequest] =
     {_specifics}

 }
  */
  def request: Parser[BuildRequest] = "request" ~> nRows ~ dataSet ~ opt(includingClause) ~ opt(specificClause) ^^ {
    case cnt ~ dsName ~ includedDs ~ specifics =>
      val specf = specifics match {
        case Some(treeReqList: List[TreeRequest]) => treeReqList
        case _ => List[TreeRequest]();
      }
      new BuildRequest(specf.head,  cnt, specf);

  }

  def nRows: Parser[Int] = wholeNumber ^^ {
    case s => s.toInt
  }

  def dataSet: Parser[String] = ident

  def singleInclude: Parser[List[String]] = dataSet ^^ { case ds => List(ds) }

  def multipleIncludes: Parser[List[String]] = "(" ~> repsep(dataSet, ",") <~ ")"

  // ^^ { case ds =>(ds)}
  def includingClause = "including" ~> (singleInclude | multipleIncludes)

  def specificClause: Parser[List[TreeRequest]] = "specific" ~ "(" ~> treeRequest <~ ")" ^^ {
    case trq => trq
  }

  def singleTreeRequest: Parser[List[TreeRequest]] = dataSet ~ "(" ~ nRows ~ ")" ~ opt("unique") ~ opt(whereClause) ~ opt(withClause) ^^ {
    case ds ~ "(" ~ rows ~ ")" ~ unique ~ conditions ~ subTrees =>
      val uniq = unique match {
        case Some(x) => true
        case _ => false;
      }
      val strees = subTrees match {
        case Some(tl: List[TreeRequest]) => tl
        case _ => List[TreeRequest]();
      }
      val cnd = conditions match {
        case Some(c: Map[String, FieldGenConstraints]) => c
        case _ => Map[String, FieldGenConstraints]();
      }
      //TreeRequest()
      List(TreeRequest ( ds, rows, cnd, None, None, false ));
  }

  def multipleTreeRequests: Parser[List[TreeRequest]] = "(" ~> repsep(treeRequest, ",") <~ ")" ^^ {
    case trList: List[TreeRequest] => trList
    case _ => Nil

  }

  def treeRequest: Parser[List[TreeRequest]] = singleTreeRequest | multipleTreeRequests;

  def whereClause: Parser[Map[String, FieldGenConstraints]] = "where" ~> fldGenSpecList ^^ {
    case l => l
  }

  def withClause: Parser[List[TreeRequest]] = "with" ~> treeRequest;

  // these
  //def withUniqClause: Parser[List[TreeRequest]] = "withUnique" ~> treeRequest;

}