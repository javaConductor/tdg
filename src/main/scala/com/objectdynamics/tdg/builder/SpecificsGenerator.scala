package com.objectdynamics.tdg.builder

import com.objectdynamics.tdg.parser.model.TreeRequest
import com.objectdynamics.tdg.spec.Relationship
import com.objectdynamics.tdg.model._
import com.objectdynamics.tdg.builder.model._
import java.util.Calendar

/**
 * Created by Object Dynamics Inc.
 *
 * User: lcollins
 * Date: 10/10/11
 * Time: 11:13 PM

 * Object Dynamics Inc. (c) 2001 - 2011
 *
 *     Just add code => 
 */

trait SpecificsGenerator
{

    this: BC2 =>

    def generateSpecifics(bldrRequest: BuildRequest, schema: ITestDataSchema, st: SymbolTable, bcState: BuildCoordinatorState): (Map[String, List[IDataRow]], BuildCoordinatorState) =
    {


        var srows: Map[String, List[IDataRow]] = Map.empty;

        var nuState = bcState;
        bldrRequest.specifics foreach
          {
              tree: TreeRequest =>
                  val dss: IDataSetSpec = schema.dataSetSpec(tree.dataSetName).get;
                  val (d, bc) = generateObjectGraphs(tree, tree.rows, bldrRequest, dss, bcState);
                  srows = addTo(srows, Map(dss.name -> d));
                  nuState = bc;
          }

        (srows, nuState)
    }


    def generateObjectGraphs(tree: TreeRequest,
                             nrows: Long,
                             bldrRequest: BuildRequest,
                             dss: IDataSetSpec,
                             bcState: BuildCoordinatorState): (List[IDataRow], BuildCoordinatorState) =
    {


        //TODO use parallel collection
        val ret: (List[IDataRow], BuildCoordinatorState) = (0L to nrows).foldLeft((List[IDataRow]() -> bcState))
                                                           {
                                                               (acc: (List[IDataRow], BuildCoordinatorState), n: Long) =>
                                                                   val (d: List[IDataRow], bc: BuildCoordinatorState) = generateObjectGraph(dss, tree, bcState);
                                                                   ((d ::: acc._1).distinct, bc);

                                                           }
        val nuState = ret._2;
        (ret._1 -> nuState)

    }


    def generateObjectGraph(dss: IDataSetSpec, tree: TreeRequest, bcState: BuildCoordinatorState): (List[IDataRow], BuildCoordinatorState) =
    {

        val st: SymbolTable = bcState.symbolTable.get;

        ///////////////////////////////////////////////////
        ////// IF this is a ref then link it to  the source
        ///////////////////////////////////////////////////
        if(tree.isAReference)
        {
            val rowList: List[IDataRow] =
                st.symbol(tree.referenceName.get) match
                {

                    case s: SymObjectList => s.value;
                    case s: SymObject => List(s.value);
                    case _ => throw new BuilderException("Unrecognized Symbol Type.");
                }


            return (rowList,

              if(tree.hasSymbolicName)
              {
                  bcState.copy(symbolTable = Some((st + (tree.symbolicName.get -> SymObjectList(rowList.map((dr: IDataRow) => () => dr))))))
              } else bcState
              );
        }

        // var nuState = bcState;
        ///////////////////////////////////////////////////
        ////// Process THIS NODE
        ///////////////////////////////////////////////////

        ///////////////////////////////////////////////////
        /// send ForeignObjectFunctionRequest to foreignBuilder
        ///	wait for  List[IDataRow]  (fdata)
        ///////////////////////////////////////////////////

        /////////
        // if this value has a symbolic name attached then wait for a value and set it in the SymbolTable
        ////////
        var nuSt = st;
        val (rowList, nuState: BuildCoordinatorState) = if(tree.hasSymbolicName)
                                                        {
                                                            val rels = relationship(tree, tree.dataSetName);
                                                            val fdataRows: List[IDataRow] = sendForeignObjectRequest_Wait(tree.dataSetName, bcState.builder(tree.dataSetName).get, tree, rels.head.fieldMap);

                                                            nuSt = fdataRows match
                                                            {
                                                                case l: List[IDataRow] => st + (dss.name -> SymObjectList(l.map((dr: IDataRow) => () => dr)))
                                                                case _ => st;
                                                            };


                                                           // val rels = relationships(tree);

                                                            val rowListAndState = tree.subTrees.foldRight((Map(tree.dataSetName -> fdataRows) -> bcState))
                                                                                  {
                                                                                      (subTree: TreeRequest,
                                                                                       acc: (Map[String, List[IDataRow]], BuildCoordinatorState)
                                                                                        ) => rels  match
                                                                                      {
                                                                                          case firstRel :: restOfRels =>
                                                                                              val (d, state) = generateSubTree(subTree, Some(firstRel), dss, acc._2);
                                                                                              (addTo(acc._1, Map(dss.name -> d)) -> state);
                                                                                          case _ => acc;
                                                                                      }

                                                                                  }
                                                            rowListAndState;
                                                        }
                                                        else // nothing for the symbolTable so just let the builder build it in the background
                                                        {
                                                            val fldMap = relationship(tree,tree.dataSetName) match {
                                                                case first :: rest => first.fieldMap
                                                                case Nil => throw new BuilderException("Dataset "+ dss.name + " has no relationship with dataSet: "+ tree.dataSetName)
                                                            }
                                                            sendForeignObjectRequest(tree.dataSetName, bcState.builder(tree.dataSetName).get, tree, fldMap);
                                                            (Map[String, List[IDataRow]]() -> bcState);
                                                        }
        (rowList(dss.name) -> nuState.copy(symbolTable = Some(nuSt)));
    };

    def applyRowsToTestData(srows: Map[String, List[IDataRow]], testData: ITestData): ITestData =
    {

        val l: List[IDataSet] = testData.dataSetList map (ds =>
            srows.get(ds.name) match
            {
                case None => ds
                case Some(rlst: List[IDataRow]) => ds + rlst;

            })

        new TestData(l);
    }

    def addTo(m1: Map[String, List[IDataRow]],
              m2: Map[String, List[IDataRow]]): Map[String, List[IDataRow]] =
    {

        val dsList = (m1.keys.toList ::: m2.keys.toList).distinct;
        (dsList map
          {
              dsName: String =>
              {

                  val vf: Function2[Map[String, List[IDataRow]], String, List[IDataRow]] = new
                      Function2[Map[String, List[IDataRow]], String, List[IDataRow]]()
                  {
                      def apply(m: Map[String, List[IDataRow]], s: String): List[IDataRow] =
                      {
                          m.get(s) match
                          {
                              case rows: List[IDataRow] => rows;
                              case _ => Nil;
                          }
                      }
                  }

                  val v1 = vf(m1, dsName)
                  val v2 = vf(m2, dsName)

                  (dsName -> (v1 ::: v2));
              }
          } toMap).asInstanceOf[Map[String, List[IDataRow]]];
    }


    def generateSubTree(tree: TreeRequest,
                        rel: Option[Relationship],
                        //parentRow: IDataRow,
                        // bldrRequest: BuildRequest,
                        dss: IDataSetSpec,
                        bcState: BuildCoordinatorState): (List[IDataRow], BuildCoordinatorState) =
    {

        val dsName = tree.dataSetName;

        // val dss: IDataSetSpec = ds.dataObjectSpec;

        var ret: List[IDataRow] = List.empty;
        val relationship = rel.get
        //var relFlds = relationship.fieldMap.values.toList

        var dr: IDataRow = new DataRow(dss, "Obj:" + dss.name + ".BC." + (Calendar.getInstance().getTimeInMillis));
        var fields: List[String] = dss.fields map ((df: IDataField) => df.name);

        fields = fields diff (relationship.fieldMap.keys toList);

        //dr = sendForeignObjectRequest(dsName, bcState.builder(dsName).get, tree, fields);

        val (d: List[IDataRow], bc: BuildCoordinatorState) = generateObjectGraph(dss, tree, bcState);
        ret = ret ::: d;

        //        tree.subTrees foreach
        //          {
        //              st: TreeRequest =>
        //              {
        //                  val rels = relationships(st);
        //                  rels.get(tree.dataSetName) match
        //                  {
        //                      case firstRel :: restOfRels =>
        //                          ret = addTo(ret, generateSubTree(tree, firstRel, ds, bcState));
        //
        //                      case _ => None;
        //                  }
        //              }
        //          }
        ret -> bc;
    }

}
