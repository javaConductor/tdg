package com.objectdynamics.tdg.builder

import com.objectdynamics.tdg.asyncmessages.ForeignObjectFunctionBCRequest
import com.objectdynamics.tdg.builder.model.{IDataField, IDataRow, IDataSet, IDataSetBuilder, IDataSetSpec}
import com.objectdynamics.tdg.builder.rdbms.ObjectGeneratorQueue
import com.objectdynamics.tdg.generators._
import com.objectdynamics.tdg.model._
import com.objectdynamics.tdg.parser.model.FieldGenConstraints
import com.objectdynamics.tdg.util._

trait Builder
  extends IDataSetBuilder {

  // with scala.actors.Actor {
  // def requestDelegate: RequestDelegate;
  //def dataSet: IDataSet;

  def dataSetSpec: IDataSetSpec;

  def isDone: Boolean;

  //def fldGenSpecToGenParm(fgs: FieldGenConstraints): GeneratorParameters;
}

sealed trait MachineState;

object Finished extends MachineState;

object Created extends MachineState;

trait RequestPending extends MachineState;

//to group pending states
object ForeignFieldFunctionRequestPending extends RequestPending;

object ForeignFieldFunctionRequestPendingAndStartRevcd extends RequestPending;

object Initialized extends MachineState;

object Started extends MachineState;


case class BuilderState(nRows: Long,
                        theQ: Option[ObjectGeneratorQueue],
                        dataSet: Option[IDataSet],
                        fieldFunctions: Map[String, ObjectFieldGenFunc],
                        foreignRequestIds: Map[String, ForeignObjectFunctionBCRequest] = Map.empty,
                        machineState: MachineState = Created) //extends Serializable
{
  def withNewRequestId(idReq: (String, ForeignObjectFunctionBCRequest)) = this.copy(foreignRequestIds = foreignRequestIds + idReq)

  def this() = this(-1L, None, None, Map.empty)

  def +(obj: IDataRow): BuilderState = this.copy(dataSet = Some((dataSet.get) + obj));

  def withNewObject(obj: IDataRow): BuilderState = this + obj;

  def withoutRequestId(reqId: String) = this.copy(foreignRequestIds = foreignRequestIds - reqId)

  def withFieldFunctions(ff: Map[String, ObjectFieldGenFunc]): BuilderState = this.copy(fieldFunctions = ff);

  //fieldFunctions = (ff ++ this.fieldFunctions).toSet.toMap);

  def withQueue(q: ObjectGeneratorQueue): BuilderState = this.copy(theQ = Some(q));

  def withDataSet(ds: IDataSet): BuilderState = this.copy(dataSet = Some(ds));

  def withRows(n: Long) = this.copy(nRows = n);

  def withMachineState(mState: MachineState) = this.copy(machineState = mState)

  def asFinished = this.copy(machineState = Finished);

}

trait genRowsFunc extends Function3[IDataSetSpec, Int, Option[Map[String, FieldGenConstraints]], List[IDataRow]] {
  def apply(dss: IDataSetSpec, rows: Int, additionalFldConstraints: Option[Map[String, FieldGenConstraints]]): List[IDataRow];
}

/**
 * Function for merging FldGenConstraints from Relationships and from TreeRequest into one
 */
trait constraintMerge extends Function2[FieldGenConstraints, FieldGenConstraints, FieldGenConstraints] {
  def apply(fgc1: FieldGenConstraints, fgc2: FieldGenConstraints): FieldGenConstraints;
}


class ValueFunction(val gv: GeneratedValue) extends (() => GeneratedValue) {
  // def this( g:GeneratedValue ) = this(() => GeneratedValue  => {g });
  // apply() = f();
  def this() = this(null);

  def apply(): GeneratedValue = {
    (if (gv == null) nullObjectFunction() else gv)
  }
}

trait DataObjectFunction extends (() => IDataRow)

trait BuilderContextSupport //extends BuilderFunctions
{
  var builderContext: BuilderContext = new BuilderContext;
  implicit val setCtxt: Function2[String, Any, Unit] = {
    (k: String, v: Any) => builderContext += (k -> v)
  }
  implicit val getCtxt: (String => Option[Any]) = {
    (k: String) => builderContext.get(k)
  }
  // type Ctxt = (Function2[String, Any, Unit], (String => Option[Any]));
  implicit val ctxt: Ctxt = Ctxt(setCtxt, getCtxt)

}

/**
 * These are the functions that actually produce the GeneratedValue instances.
 *
 * There are 2 subclasses:
 * LocalGenFunction
 * and
 * ForeignGenFunction
 */
trait ObjectFieldGenFunc extends ((String, Long) => (ValueFunction)) {

  def apply(fldName: String, rowIdx: Long): (ValueFunction)

  def next = this;


  def whichSet(nn: Int, sizes: List[Int]): Int = {

    var s: Int = -1;
    var slist: List[Int] = sizes.tail;
    val listSum: Int = sizes.sum
    var n = nn;
    while (n > listSum) {
      n -= listSum;
    }

    while (s == -1 && !slist.isEmpty) {
      if (n >= ((slist sum) toInt)) {
        s = slist size
      }
      else {
        slist = slist.tail
      };
    }
    if (s == -1) {
      0
    }
    else {
      s
    };
  }
}

//trait Ctxt extends Tuple2[ Function2[String, Any, Unit], (String => Option[Any]) ]    {
//
//}
case class Ctxt(wr: Function2[String, Any, Unit], rd: String => Option[Any])
  extends Tuple2[Function2[String, Any, Unit], (String => Option[Any])](wr, rd) {

  def apply(k: String, v: Any): Unit = this._1.apply(k, v);

  def apply(k: String): Option[Any] = this._2.apply(k);

}

class LocalGenFunction(fld: IDataField,
                       fldGenList: FieldGenList,
                       ofgs: Option[FieldGenConstraints],
                       totalObjectsToGen: Long,
                       implicit val ctxt: Ctxt
                        )
  extends ObjectFieldGenFunc with NoLogContributor {
  def apply(fldName: String, rowIdx: Long): ValueFunction = {

    val normalGen: FieldGeneratorFunction = (fldGenList.forField(fld, ofgs) get);
    log("LocalGenFunction(" + fld + ").(String, Long): using FieldGenFunc: " + normalGen);

    val v: ValueFunction = ofgs match {
      case Some(fgs: FieldGenConstraints) =>
        // init the generator
        //        normalGen.init(ctxt, fld, totalObjectsToGen)(fgs)

        normalGen.init(ctxt, fld, totalObjectsToGen, Some(fgs))

        normalGen.apply(ctxt, fld, Some(fgs));

      case _ =>
        normalGen.init(ctxt, fld, totalObjectsToGen, None)
        normalGen.apply(ctxt, fld, None);
    }

    v;
  };

}

case class ForeignGenFunction(fldName: String, fldFunctions: List[ValueFunction], disb: Disbursement)
  extends ObjectFieldGenFunc {
  def apply(fldName: String, rowIdx: Long): ValueFunction = {
    disb.disburse(fldFunctions)
  };

  override
  def next = {
    // advance the disbursement
    this.copy(disb = disb.advance)
  }
}

trait SymbolValueObj;

/**
 * These objects represent requestSymbolTable values
 *
 */
trait SymbolValue[A] extends SymbolValueObj {

  trait SymType;

  object DataObject extends SymType;

  object DataObjectList extends SymType;

  object DataField extends SymType;

  val symType: SymType;

  def value: A;

};

case class SymObject(objFunc: () => IDataRow) extends SymbolValue[IDataRow] {
  val symType: SymType = DataObject;

  def value = objFunc();
};

case class SymObjectList(
                          objFuncList: List[() => IDataRow]
                          ) extends SymbolValue[List[IDataRow]] {
  val symType: SymType = DataObjectList;

  def value = List[IDataRow]();
}

case class SymObjectField(obj: SymObject, fieldName: String) extends SymbolValue[ValueFunction] {
  val symType: SymType = DataField;

  def value = obj.value.value(fieldName);
}

class SymbolTable(symbols: Map[String, SymbolValueObj]) {

  def this() = this(Map[String, SymbolValueObj]());

  def +(symAndName: Tuple2[String, SymbolValueObj]): SymbolTable = {
    new SymbolTable(symbols + symAndName);
  }

  def symbol(symbolName: String): Option[SymbolValueObj] = {
    symbols.get(symbolName)
  }

}

