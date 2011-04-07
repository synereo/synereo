// -*- mode: Scala;-*- 
// Filename:    PersistedMonadicTermStoreTests.scala 
// Authors:     lgm                                                    
// Creation:    Tue Apr  5 20:08:45 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.test.store

import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner

import org.junit._

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.store.xml.datasets._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.util.continuations._ 
import scala.xml._

import org.prolog4j._

import org.basex.api.xmldb.BXCollection

import org.xmldb.api.base.{ Resource => XmlDbRrsc, _}
import org.xmldb.api.modules._
import org.xmldb.api._

//import org.exist.util.serializer.SAXSerializer
//import org.exist.util.serializer.SerializerPool

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import javax.xml.transform.OutputKeys
import java.util.Properties
import java.net.URI
import java.util.UUID


class FromXMLTest
extends JUnit3(FromXMLTestSpecs)

object FromXMLTestSpecsRunner
extends ConsoleRunner(FromXMLTestSpecs)


object FromXMLTestSpecs extends Specification {
  import PersistedMonadicTS._
  "Pattern as query" should {
    "Retrieve 3 values from the GraphFour DB" in {
      BX.reportGraphs
      val ptn1 = "record(" + BX.outerGraphExpr + "," + "V" + ")"
      val pimgJunq = ptToPt( "GraphFour", "localhost", "localhost" )
      val atps = pimgJunq.agentTwistedPairs
      val ccl1 =
	CXQ.fromCaseClassInstanceString(
	  ptn1
	).getOrElse(
	  null
	).asInstanceOf[CnxnCtxtLabel[String,String,String]]
      val qry1 = CXQ.xqQuery( ccl1 )
      val bx1 = new BXCollection( "GraphFour", true )
      val xqSrvc =
	bx1.getService(
	  "XPathQueryService", "1.0"
	).asInstanceOf[XPathQueryService]
      val rs2 = xqSrvc.query( qry1 )
      val rsSz = rs2.getSize
      val rs2Iter = rs2.getIterator
      val rsrc1 = rs2Iter.nextResource
      val rsrc1Str = rsrc1.getContent.toString
      val elem1 = XML.loadString( rsrc1Str )
      val ccl2 =
	CXQ.fromXML(
	  ( x : String ) => x,
	  ( x : String ) => x,
	  ( x : String ) => x
	)( elem1 ).getOrElse( null )
      println( ccl2.toString )
      //assertEquals( 3, rsSz )
    }
  }
}
