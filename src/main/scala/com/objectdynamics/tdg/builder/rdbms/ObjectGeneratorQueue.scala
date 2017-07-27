package com.objectdynamics.tdg.builder.rdbms

import java.lang._
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}

import com.objectdynamics.tdg.asyncmessages._
import com.objectdynamics.tdg.builder.model.IDataRow
import com.objectdynamics.tdg.util._

import scala.actors.Actor
import scala.annotation.tailrec
import scala.concurrent.stm._

/**
  * Created by IntelliJ IDEA.
  * User: lcollins
  * Date: 8/2/11
  * Time: 12:03 AM
  *
  */

case class QueueState(fldFuncs: Map[String, ObjectFieldGenFunc],
                      postProcess: Option[(IDataRow) => IDataRow],
                      q: BlockingQueue[ObjectGeneration] = new ArrayBlockingQueue[ObjectGeneration](1000));

class ObjectGeneratorQueue(postProcess: Option[(IDataRow) => IDataRow])
  extends NoLogContributor
    with Actor {

  val state = Ref(new QueueState(Map.empty, postProcess));
  var generated = 0L;
  var isShuttingDown = false;

  def act {
    log("ObjectGeneratorQueue.act() - started!");
    loop {
      react {
        case "exit" => {
          this.exit();
          log("ObjectGeneratorQueue.act() - exiting!");
        }

        case AddFunks(ff: Map[String, ObjectFieldGenFunc]) =>
          atomic {
            implicit txn => // nested atomic
              state.transform((qs: QueueState) => qs.copy(fldFuncs = qs.fldFuncs ++ ff));
              log("ObjectGeneratorQueue.act() - AddFunks(" + ff + ")");
              null;
          }


        case og: ObjectGeneration =>
          log("ObjectGeneratorQueue.act() recvd msg:" + og);
          atomic {
            implicit txn =>
              state().q.put(og);


          }
          log("ObjectGeneratorQueue.act() Q " + state.single.get.q + " size:" + state.single.get.q.size())

        case StartBuild => startQ
        case qIn: BlockingQueue[ObjectGeneration] => atomic {
          implicit txn => // nested atomic
            state.transform((qs: QueueState) => qs.copy(q = qIn));


        }
        case Stop => isShuttingDown = true;

      }
    }

  }

  @tailrec
  final def generate(fList: Map[String, ObjectFieldGenFunc], objIdx: Long = 1): Unit = {
    log("ObjectGeneratorQueue.generate(" + fList + ",  " + isShuttingDown + "," + objIdx + ")");
    log("ObjectGeneratorQueue.generate() Q:" + this + " size:" + state.single.get.q.size())
    Thread.sleep(500);
    var objGen = state.single.get.q.peek() //poll( 1L,TimeUnit.SECONDS);
    while (objGen == null && !isShuttingDown) {
      Thread.sleep(200);
      log("ObjectGeneratorQueue.generate(" + objIdx + ") Q size:" + state.single.get.q.size() + " got null - polling again...");
      objGen = state.single.get.q.peek();
    }

    log("ObjectGeneratorQueue.generate(" + objIdx + ") got msg:" + objGen + (if (isShuttingDown) " SHUTTING DOWN!" else ""));
    if (!isShuttingDown) {
      state.single.get.q.take();
      val nuObj = generateObject(objGen, fList, objIdx);
      log("ObjectGeneratorQueue.generate(" + objIdx + ")=" + nuObj + " -> objGen PostProcess");

      val semiFinalObj = objGen.postProcess match {

        case Some(x: ((IDataRow) => IDataRow)) => {
          x(nuObj)
        }
        case None => {
          log("ObjectGeneratorQueue.generate(" + objIdx + ") -> no objGen PostProcess function.");

          nuObj
        }
      }
      log("ObjectGeneratorQueue.generate(" + objIdx + ") semiFinalObj=" + semiFinalObj + " ->  global PostProcess");
      val finalObj = state.single.get.postProcess match {

        case Some(x: ((IDataRow) => IDataRow)) => {
          x(nuObj)
        }
        case None => {
          semiFinalObj
        }
      }
      generated += 1;
      log("ObjectGeneratorQueue.generate(" + objIdx + ") produced Object: " + finalObj);
      if (!isShuttingDown)
        generate(cycleFuncs(fList), objIdx + 1);
      else
        fList; //log("ObjectGeneratorQueue.generate("+objIdx+") Q size:"+state.single.get.q.size()+" exiting !" );
    }
  }

  protected def startQ = {
    log("ObjectGeneratorQueue.startQ()");
    val me = this;
    class GenRun extends Runnable {
      def run() {
        generate(state.single.get.fldFuncs);
        log("ObjectGeneratorQueue.run() - exiting.");
        // exit();
        me ! "exit";
      }
    }

    new Thread(new GenRun).start();
    log("ObjectGeneratorQueue.startQ() - Spawned generate() function.");
  }

  protected def generateObject(objGen: ObjectGeneration, fldFuncs: Map[String, ObjectFieldGenFunc], objIdx: Long): IDataRow = {
    log("ObjectGeneratorQueue.generateObject(" + fldFuncs + ")");

    fldFuncs.foldLeft[IDataRow](objGen.rowSoFar)((acc: IDataRow, optVal: Tuple2[String, ObjectFieldGenFunc]) => {
      val o: IDataRow = acc + (optVal._1 -> optVal._2.apply(optVal._1, objIdx));
      log("ObjectGeneratorQueue.generateObject() generating " + optVal._1 + " for obj:" + acc.id + "=" + o);
      o;
    });

  }

  private def cycleFuncs(origFuncs: Map[String, ObjectFieldGenFunc]): Map[String, ObjectFieldGenFunc] = {

    val nuFuncList = cycleFuncs3(origFuncs.toList);
    val nuFuncs: Map[String, ObjectFieldGenFunc] = nuFuncList map { funcTpl => (funcTpl._1, funcTpl._2) } toMap;
    nuFuncs;
  }

  @tailrec
  private def cycleFuncs3(origFuncs: List[(String, ObjectFieldGenFunc)],
                          funcsSoFar: List[(String, ObjectFieldGenFunc)] = List[(String, ObjectFieldGenFunc)]()
                         ): List[(String, ObjectFieldGenFunc)] = {

    origFuncs match {
      case Nil => funcsSoFar.reverse;
      case func :: theRest => cycleFuncs3(theRest, (func._1, func._2.next) :: funcsSoFar);
    }
  }


}