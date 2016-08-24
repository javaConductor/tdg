package com.objectdynamics.tdg.spec

import com.objectdynamics.tdg.builder.BuilderException

/**
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: Nov 21, 2010
 * Time: 1:02:40 AM
 * To change this template use File | Settings | File Templates.
 */
/**
 * A disbursement can be define in 2 ways:
 *  pct:[30,30,20,20]
 *      and
 *  ratio:[3,1]
 *
 */

object DisbursementSpec
{
    def createPercentages(nRows: Long, rootRowCount: Int, relatedRowCount: Int): List[Double] =
    {
        val fCnt = rootRowCount * relatedRowCount / nRows;
        val fullPcts: Long = nRows / fCnt;
        val partialPct: Long = (nRows % fCnt);
        val pctValue: Double = 100.0 / fullPcts;
        //val partialPctValue = 100 % fullPcts;
        var pcts: List[Double] = List.make(fullPcts.asInstanceOf[Int], pctValue);
        if(partialPct > 0)
        {
            pcts = partialPct :: pcts
        };

        pcts;
    }
}

class DisbursementSpec()
{
    private var percentages: Option[List[Double]] = None;
    private var ratio: Option[Tuple2[Double, Double]] = None;


    def this(p: List[Double]) =
    {
        this ();
        percentages = Some(p map
                             {_ / 100});
    }

    /**
     * A Tuple2 containing: Ratio eg. 50 to 1 or 3 to 2
     * Meaning: there is one unique value in the corresponding dataSet for every 50 withRows
     */
    def this(r: Tuple2[Double, Double]) =
    {
        this ();
        ratio = Some(r);
    }

    def getPercentages(nRows: Long): List[Double] =
    {
        percentages match
        {
            case Some(p) => p
            case None => createPercentages(nRows, foreignRowsRequired(nRows))
        }
    }

    def createPercentages(nRows: Long, nDisbursed: Long): List[Double] =
    {
        val npct: Int = (nRows / nDisbursed).asInstanceOf[Int];
        val pct: Double = npct.asInstanceOf[Double] / nRows.asInstanceOf[Double];

        createDoubleList(nDisbursed.asInstanceOf[Int], pct, List[Double]());

    }

    private def createDoubleList(cnt: Int, value: Double, acc: List[Double]): List[Double] =
    {
        // add code for pct correction!!!
        cnt match
        {
            case 0 => acc;
            case n: Int => createDoubleList(n - 1, value, value :: acc);
        }
    }

    def dataItemsRequired(nRows: Long): Long =
    {
        percentages match
        {
            case Some(p) => (p.size).asInstanceOf[Long];
            case _ => ratio match
            {
                case Some(r: Tuple2[Double, Double]) =>
                    val n = (nRows / (r._1 / r._2));
                    //percentages = Some(createPercentages(nRows, n));
                    Math.max(1L, (nRows / (r._1 / r._2)).asInstanceOf[Long])
                case _ => throw new BuilderException("Bad DisbursementSpec: must have percentages or ratio.");
            }
        }
    }

    def foreignRowsRequired(nRows: Long): Long = dataItemsRequired(nRows);

}