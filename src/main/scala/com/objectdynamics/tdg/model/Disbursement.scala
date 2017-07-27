package com.objectdynamics.tdg.model

import com.objectdynamics.tdg.spec.DisbursementSpec;

/**
  * Created by IntelliJ IDEA.
  * User: Lee
  * Date: 12/16/10
  * Time: 12:37 AM
  * To change this template use File | Settings | File Templates.
  */

trait Disbursement {
  def advance: Disbursement;

  @deprecated
  def indices: List[Long];

  def currentSet: CurrentSet;

  def disbursementSpec: DisbursementSpec;

  def disburse[T](in: List[T]): T;

  def roundRobin(dataSize: Long): Disbursement = {
    roundRobinDisbursement(dataSize);
  }

  def createStateMap(spec: DisbursementSpec, nRows: Long): Map[Int, Long];
}

case class roundRobinDisbursement(dataSize: Long, current: Long = 0) extends AbstractDisbursement {
  val _currentSet: CurrentSet = CurrentSet(current.intValue())

  def createStateMap(spec: DisbursementSpec, nRows: Long): Map[Int, Long] = {

    (1L to dataSize) map { l: Long => (l.intValue(), 1L) } toMap
  }

  def advance: Disbursement = {
    val nuc = if (current >= (dataSize.toInt - 1)) 0 else current + 1
    copy(current = nuc);
  }

  def indices: List[Long] = {
    1L :: List(current)
  };

  def currentSet: CurrentSet = _currentSet

  def disbursementSpec: DisbursementSpec = null;

}

case class CurrentSet(setIdx: Int, subSetIdx: Long = 0);