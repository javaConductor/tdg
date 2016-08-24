package com.objectdynamics.tdg.builder.rdbms

import com.objectdynamics.tdg.parser.model._
import com.objectdynamics.tdg.builder.model._
import com.objectdynamics.tdg.builder.BuilderException

/**
 * Created by Object Dynamics Inc.
 *
 * User: lcollins
 * Date: 10/30/11
 * Time: 2:53 AM
 
 * Object Dynamics Inc. (c) 2001 - 2011
 *
 *     Just add code => 
 */

trait TreeRequestBuilder {
    def mainTreeRequest(rootObj: String, nRows: Long, dss:IDataSetSpec): TreeRequest =
     {

         val fgcMap: Map[String, FieldGenConstraints] =
             dss.fields.foldRight(Map[String, FieldGenConstraints]())
             {
                 (df: IDataField, acc: Map[String, FieldGenConstraints]) =>
                     acc + fieldToFieldConstraintMap(df);
             }


         new TreeRequest(dss.name, nRows, fgcMap, None, None, Nil)

     }


     def fieldToFieldConstraintMap(df: IDataField): Tuple2[String, FieldGenConstraints] =
     {


         var fldConstraintList: List[FieldGenConstraint] = List.empty;
         val betweenSpec = for (mx <- df.max; mn <- df.min)
         yield BetweenSpec(mn.toString(), mx.toString());
         if(betweenSpec != None) fldConstraintList = betweenSpec.get :: fldConstraintList;

         val inSpec: Option[FieldGenConstraint] = df.data match
         {
             case x: List[String] => if(x.isEmpty) None else Some(InSpec(x));
             case _ => None;
         };
         if(inSpec != None) fldConstraintList = inSpec.get :: fldConstraintList;

         val eqSpec: Option[FieldGenConstraint] = df.data match
         {
             case x: List[String] => if(x.size == 1) Some(EqSpec(x(0))) else None;
             case _ => None;
         };
         if(eqSpec != None) fldConstraintList = eqSpec.get :: fldConstraintList;

         var fldConstraints: FieldGenConstraints = new FieldGenConstraints(df.name, fldConstraintList);
         (df.name -> fldConstraints)
     }


}