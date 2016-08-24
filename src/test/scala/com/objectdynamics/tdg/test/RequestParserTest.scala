package com.objectdynamics.tdg.test

;


//import org.junit.Assert;

import org.junit.runner.RunWith
import org.specs.runner.JUnitSuiteRunner
import org.specs._
import com.objectdynamics.tdg.parser.RequestParser
import com.objectdynamics.tdg.builder._

@RunWith(classOf[JUnitSuiteRunner])
class RequestParserTest extends SpecificationWithJUnit
{


    /*
    request 100 Customer including ()
    specific(
        Customer(7) where age > 21 with
    (Purchase(12) where date > 1999 / 12 / 31 with
    PurchaseItem(11) where price > 20.25 and qty = 1 and
            Purchase(1) where date < 1999 / 12 / 31 with
    PurchaseItem(1) where price > 25.75 and qty = 1
    ) and
    Customer(7) where age < 16 with
    (Purchase(3) where date > 1999 / 12 / 31 with
    PurchaseItem(75) where price > 20.25 and qty = 1 and
            Purchase(5) where date < 1999 / 12 / 31 with
    PurchaseItem(1) where price > 25.75 and qty = 1
    )

    )*/
    "can parse simple request" in
      {
          val optReq: Option[BuildRequest] = RequestParser.parseRequest(

              """ request 100 Customer including Purchase"""
          )
          optReq must beSome[BuildRequest];
          val req = optReq.get;
          System.out.println(req.toString);
          req.nRows must be(100);
      }

    "can parse simple request w specific" in
      {
          val optReq: Option[BuildRequest] = RequestParser.parseRequest(

              """ request 100 Customer including Purchase
        specific(
         Customer(7) where age between 21,75 )"""
          )
          optReq must beSome[BuildRequest];
          val req = optReq.get;
          System.out.println(req.toString);
          req.nRows must be(100);
      }
      
    "can parse simple request w specific w 2 fldgenspecs" in
      {
          val optReq: Option[BuildRequest] = RequestParser.parseRequest(

              """ request 100 Customer including Purchase
        specific(
        Customer(7) where age between 21,75+between 210,750 )"""
          )
          optReq must beSome[BuildRequest];
          val req = optReq.get;
          System.out.println(req.toString);
          req.nRows must be(100);
      }
    "can parse simple request w specific w date" in
      {
          val optReq: Option[BuildRequest] = RequestParser.parseRequest(

              """ request 100 Customer including Purchase
        specific(
         Customer(7) where joinDate between 2000/12/31,2009/12/31 )"""
          )
          optReq must beSome[BuildRequest];
          val req = optReq.get;
          System.out.println(req.toString);
          req.nRows must be(100);
      }

    "can parse simple request w specific w 2 dates" in
      {
          val optReq: Option[BuildRequest] = RequestParser.parseRequest(

              """ request 100 Customer including Purchase
        specific(
        Customer(7) where joinDate between 2000/12/31,2009/12/31+between 1966/12/28,1984/06/12 )"""
          )
          optReq must beSome[BuildRequest];
          val req = optReq.get;
          System.out.println(req.toString);
          req.nRows must be(100);
      }


    "can parse simple request w specific w multiple levels" in
      {
          val optReq: Option[BuildRequest] = RequestParser.parseRequest(

              """ request 100 Customer including Purchase
          specific(
              Customer(7)
                  where joinDate between 2000/12/31,2009/12/31+between 1966/12/28,1984/06/12
                  with
                  (Purchase(12)
                      where date between 2000/12/31,2009/12/31+between 1966/12/28,1984/06/12
                      with
                      (PurchaseItem(11)
                          where price between 0,20.25 and qty eq 1
                      )
                  )
              )"""
          )
          optReq must beSome[BuildRequest];
          val req = optReq.get;
          System.out.println(req.toString);
          req.nRows must be(100);
      }
}
