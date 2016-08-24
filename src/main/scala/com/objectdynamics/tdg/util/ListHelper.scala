package com.objectdynamics.tdg.util

/**
 * Created by IntelliJ IDEA.
 * User: Lee
 * Date: 12/15/10
 * Time: 5:38 PM
 * To change this template use File | Settings | File Templates.
 */

trait ListUtil extends ConverterFunks
{
    implicit def listToMap[K, V](l: List[V], getKey: (V => K)): Map[K, V] =
    {
        l map
          {v: V => (getKey(v) -> v)} toMap;
    }

    implicit def iteratorToList[B](itr: Iterator[B]): List[B] =
    {
        itr.toIndexedSeq.toList
    }

    implicit def iterableToList[B](itr: Iterable[B]): List[B] =
    {
        itr.toIndexedSeq.toList
    }

    def mapFromTuplesList[K, V](l: List[Tuple2[K, V]]): Map[K, V] =
    {
        (l map
          {tpl: (K, V) => (tpl._1 -> tpl._2)}) toMap

    }

    implicit def listToSet[V](l: List[V]): Set[V] =
    {
        //l.toSeq.IndexedSeq.toList
        l.foldLeft(Set[V]())((r, c) => r + c)
    }

    def changeOptionType[F, T](l: List[Option[F]])
                              (implicit conversionFunk: (Option[F] => Option[T])): List[Option[T]] =
    {
        l map (optF =>
        {
            conversionFunk(optF)
        });

    }

}

object ListHelper extends ListUtil
{

}

object FRowPrefix
{
    def apply(requestingField: String, dataSetName: String): String =
    {
        "frows:" + requestingField + ":" + dataSetName
    }
}