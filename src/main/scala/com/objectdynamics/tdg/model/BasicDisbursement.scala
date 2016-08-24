package com.objectdynamics.tdg.model

import com.objectdynamics.tdg.spec.DisbursementSpec
import com.objectdynamics.tdg.util._

/**
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: Nov 21, 2010
 * Time: 1:02:40 AM
 * To change this template use File | Settings | File Templates.
 */
object BasicDisbursement
{

}

trait AbstractDisbursement extends Disbursement with NoLogContributor{
     def disburse[T](in: List[T]): T =
    {
        log("Disbursement.disburse("+in+") currentSet:"+currentSet+" setIdx:"+this.currentSet.setIdx+"");
        var idx = this.currentSet.setIdx % in.size;

        log("Disbursement.disburse("+in+") idx:"+this.currentSet.setIdx+"");
        val v: T = in(idx);

        log("Disbursement.disburse("+in+") = " + v);
        v
    }
};

class BasicDisbursement(val spec: DisbursementSpec, val nRows: Long, val setIdx: Int,
                        val elementIdx: Long) extends AbstractDisbursement
{
    validate

    val _currentSet: CurrentSet = CurrentSet(setIdx, elementIdx)

    def this(spec: DisbursementSpec, nRows: Long) =
    {
        this (spec, nRows, 0, 0);
    }

    def validate() =
    {
        /// if min/max limit to number to below the desired amount
        /// if rotatedString conflicts with
    }

    private val stateMap: Map[Int, Long] = createStateMap(spec, nRows);

    def createStateMap(spec: DisbursementSpec, nRows: Long): Map[Int, Long] =
    {
        var m = Map[Int, Long]();
        var subTtl = 0.0;
        val dRows: Double = nRows.asInstanceOf[Double];
        val percentages = spec.getPercentages(nRows);
        log("BasicDisbursement:statemap: pcts:" + percentages);
        percentages.//dropRight(1).
        zipWithIndex.
        foreach
        {
            case (d: Double, i) =>
            {
                //                               m = m + (i -> (dRows * d / 100.0).asInstanceOf[Int])
                val bd: scala.math.BigDecimal = scala.math.BigDecimal(dRows * d);
                bd.setScale(0, scala.math.BigDecimal.RoundingMode.HALF_UP);
                m = m + (i -> bd.longValue)
                subTtl = subTtl + (bd.intValue).asInstanceOf[Int];
            }
        }


        //// re-distribute -neg or +pos
        // pos: if 50 are generated when 53 are needed then distibute 3 more across the first 3 buckets
        // neg: if 50 are generated when 48 are needed then decrease the count of 2 across the first 2 buckets

         val diff = nRows - subTtl.toLong;
       val m2: Map[Int, Long]  = (if (diff > 0){
         m map {element:(Int , Long) =>  if (element._1 < diff) (element._1, element._2+1) else element}
        }else
        if (diff < 0){
           m map {element:(Int , Long) =>  if (element._1 < diff) (element._1, element._2 - 1) else element} }
        else m
         toMap)


        //the last one is prorated
//        m = m + (percentages.length - 1 -> (nRows - subTtl.toLong));
//        log("BasicDisbursement:statemap:" + m);

        m2
    }

    def advance: Disbursement =
    {
        var e: Long = elementIdx;
        var s: Int = setIdx;
        log("BasicDisbursement.advance: start: e,s=" + (e, s));

        stateMap.get(setIdx) match
        {
            // now we found the set so which iteration of the set
            case Some(elementCount) =>
                // log("BasicDisbursement.advance: elementCount=" + (elementCount));
                if(elementIdx >= elementCount - 1)
                {
                    e = 0;
                    if(stateMap.isDefinedAt(s + 1))
                    {
                        s = s + 1;
                    }
                    else
                    {
                        //   log("BasicDisbursement.advance: statemap not def @=" + (s + 1));
                        s = 0;
                    };
                }
                else
                {
                    e = e + 1;
                }
            case None =>
                e = 0;
                s = 0;
        }
        log("BasicDisbursement.advance: end: e,s=" + (e, s));
        new BasicDisbursement(spec, nRows, s, e);
    };


    //    def disburse[T](in: List[T]): T = {
    //
    //        var idx = this.setIdx;
    //        while (idx > in.size)
    //            idx -= in.size;
    //        val v:T = in(idx);
    //        v
    //    }

    def indices: List[Long] = List[Long](setIdx, elementIdx);

    def currentSet: CurrentSet = _currentSet

    def set: Int = setIdx;

    def element: Long = elementIdx;

    def disbursementSpec: DisbursementSpec = spec;

}