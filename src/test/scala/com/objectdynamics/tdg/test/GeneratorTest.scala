package com.objectdynamics.tdg.test

import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner
import org.specs._
import com.objectdynamics.tdg.spec.datatypes._
import com.objectdynamics.tdg.generators._
import com.objectdynamics.tdg.spec._
import com.objectdynamics.tdg.builder._
import com.objectdynamics.tdg.builder.model._

@RunWith(classOf[JUnitSuiteRunner])
class GeneratorTest extends SpecificationWithJUnit
{
    var ctxtMap: Map[String, Any] = Map[String, Any]();

    def setCtxt(k: String, v: Any) =
    {
        ctxtMap += (k -> v);
    }

    def getCtxt(k: String): Option[Any] =
    {
        ctxtMap.get(k);
    }

    val ctxt: Ctxt = new Ctxt(setCtxt, getCtxt);

    implicit val emptyGenParms: GeneratorParameters = DefaultGenParameters;
    "can generate Rotating Strings" in
      {
          var rotatingFldData: List[String] = List("Genesis", "Exodus", "Leviticus", "Numbers",
                                                   "Deuteronomy");
          val rotatingStringField: IDataField = DataField("rstr", Text, None, None, None, None, None, false, Some(rotatingFldData), Some(new
                                                                                                                                             DisbursementSpec(List(20.0, 20.0, 20.0, 20.0, 20.0))));

          // ctxt = ctxt.empty;
          var cntMap: Map[String, Int] = rotatingFldData map ((s: String) => (s -> 0)) toMap;
          val r: FieldGeneratorFunction = new RotateString;

          r.init(ctxt, rotatingStringField, 100, None);


          (0 until 100).foreach
          {
              n: Int =>
                  var gVal: ValueFunction = r(ctxt, rotatingStringField, None);
                  System.out.println("rotatingStringField => " + gVal.gv.value.getValue().toString);
                  gVal mustNotBe null;

                  val tv: TextValue = gVal().asInstanceOf[TextValue];
                  var s: String = tv.value match
                  {
                      case Some(x: String) => x;
                      case _ => ""
                  };


                  //s must beIn rotatingFldData;
                  //rotatingFldData must contain s
                  rotatingFldData.contains(s) must beTrue

                  val cnt: Int = cntMap(s);

                  cntMap += (s -> (cnt + 1));

          }
          cntMap("Genesis") must be(20);
          cntMap("Exodus") must be(20);
          cntMap("Leviticus") must be(20);
          cntMap("Numbers") must be(20);
          cntMap("Deuteronomy") must be(20);

      }



    "can generate Rotating Strings w 0 in spec" in
      {
          var rotatingFldData: List[String] = List("Genesis", "Exodus", "Leviticus", "Numbers",
                                                   "Deuteronomy");
          val rotatingStringField: IDataField = DataField("rstr", Text, None, None, None, None, None, false, Some(rotatingFldData), Some(new
                                                                                                                                            DisbursementSpec(List(20.0, 20.0, 20.0, 20.0, 20.0))));

          var cntMap: Map[String, Int] = rotatingFldData map ((s: String) => (s -> 0)) toMap;
          val r: FieldGeneratorFunction = new RotateString;
          r.init(ctxt, rotatingStringField, 53, None);

          (0 until 53).foreach
          {
              n: Int =>
                  var gVal: ValueFunction = r(ctxt, rotatingStringField, None);
                  System.out.println("rotatingStringField => " + gVal().value);
                  gVal mustNotBe null;

                   val tv: TextValue = gVal().asInstanceOf[TextValue];
                  var s: String = tv.value match
                  {
                      case Some(x: String) => x;
                      case _ => ""
                  };

                  //s must beIn rotatingFldData;
                  //rotatingFldData must contain s
                  rotatingFldData.contains(s) must beTrue

                  val cnt: Int = cntMap(s);

                  cntMap += (s -> (cnt + 1));

          }

            cntMap("Genesis") must be(11);
          cntMap("Exodus") must be(11);
          cntMap("Leviticus") must be(11);
          cntMap("Numbers") must be(10);
          cntMap("Deuteronomy") must be(10);
          System.out.println(cntMap("Genesis"))
          System.out.println(cntMap("Exodus"))
          System.out.println(cntMap("Leviticus"))
          System.out.println(cntMap("Numbers"))
          System.out.println(cntMap("Deuteronomy"))

      }


    "can generate Rotating Strings w neg distribution" in
      {
          var rotatingFldData: List[String] = List("Genesis", "Exodus", "Leviticus", "Numbers",
                                                   "Deuteronomy");
          val rotatingStringField: IDataField = DataField("rstr", Text, None, None, None, None, None, false, Some(rotatingFldData), Some(new
                                                                                                                                            DisbursementSpec(List(20.0, 20.0, 20.0, 20.0, 20.0))));

          var cntMap: Map[String, Int] = rotatingFldData map ((s: String) => (s -> 0)) toMap;
          val r: FieldGeneratorFunction = new RotateString;
          r.init(ctxt, rotatingStringField, 48, None);

          (0 until 48).foreach
          {
              n: Int =>
                  var gVal: ValueFunction = r(ctxt, rotatingStringField, None);
                  System.out.println("rotatingStringField => " + gVal().value);
                  gVal mustNotBe null;

                   val tv: TextValue = gVal().asInstanceOf[TextValue];
                  var s: String = tv.value match
                  {
                      case Some(x: String) => x;
                      case _ => ""
                  };

                  //s must beIn rotatingFldData;
                  //rotatingFldData must contain s
                  rotatingFldData.contains(s) must beTrue

                  val cnt: Int = cntMap(s);

                  cntMap += (s -> (cnt + 1));

          }

             System.out.println(cntMap("Genesis"))
          System.out.println(cntMap("Exodus"))
          System.out.println(cntMap("Leviticus"))
          System.out.println(cntMap("Numbers"))
          System.out.println(cntMap("Deuteronomy"))
            cntMap("Genesis") must be(10);
          cntMap("Exodus") must be(10);
          cntMap("Leviticus") must be(10);
          cntMap("Numbers") must be(9);
          cntMap("Deuteronomy") must be(9);


      }


    "can generate Rotating Strings w neg distribution and odd disbursement" in
      {
          var rotatingFldData: List[String] = List("Genesis", "Exodus", "Leviticus", "Numbers",
                                                   "Deuteronomy");
          val rotatingStringField: IDataField = DataField("rstr", Text, None, None, None, None, None, false, Some(rotatingFldData), Some(new
                                                                                                                                            DisbursementSpec(List(10.0, 20.0, 30.0, 15.0, 25.0))));

          var cntMap: Map[String, Int] = rotatingFldData map ((s: String) => (s -> 0)) toMap;
          val r: FieldGeneratorFunction = new RotateString;
          r.init(ctxt, rotatingStringField, 100, None);

          (0 until 100).foreach
          {
              n: Int =>
                  var gVal: ValueFunction = r(ctxt, rotatingStringField, None);
                  System.out.println("rotatingStringField => " + gVal().value);
                  gVal mustNotBe null;

                   val tv: TextValue = gVal().asInstanceOf[TextValue];
                  var s: String = tv.value match
                  {
                      case Some(x: String) => x;
                      case _ => ""
                  };

                  //s must beIn rotatingFldData;
                  //rotatingFldData must contain s
                  rotatingFldData.contains(s) must beTrue

                  val cnt: Int = cntMap(s);

                  cntMap += (s -> (cnt + 1));

          }

             System.out.println(cntMap("Genesis"))
          System.out.println(cntMap("Exodus"))
          System.out.println(cntMap("Leviticus"))
          System.out.println(cntMap("Numbers"))
          System.out.println(cntMap("Deuteronomy"))
            cntMap("Genesis") must be(10);
          cntMap("Exodus") must be(20);
          cntMap("Leviticus") must be(30);
          cntMap("Numbers") must be(15);
          cntMap("Deuteronomy") must be(25);


      }
}
