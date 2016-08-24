package com.objectdynamics.tdg.util

trait ConverterFunks
{

    implicit def convertStr2Dbl(f: Option[String]): Option[Double] =
    {

        f match
        {
            case Some(s: String) => Some(s.toDouble)
            case None => None
        }

    }

    implicit def convertStr2Bd(f: Option[String]): Option[BigDecimal] =
    {

        f match
        {
            case Some(s: String) => Some(s.toDouble)
            case None => None
        }

    }

    implicit def flip[A, B](m: Map[A, B]): Map[B, A] =
    {
        val r: Map[B, A] = m map
          {
              (t: Tuple2[A, B]) =>
                  (m(t._1), t._1)
          } toMap;
        r;
    }


    implicit def convertDbl2Dbl(f: Option[Double]): Option[Double] = f

}