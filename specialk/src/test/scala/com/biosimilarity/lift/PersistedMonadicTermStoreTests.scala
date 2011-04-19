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


class PersistedMonadicTermStoreTest
extends JUnit3(PersistedMonadicTermStoreTestSpecs)

object PersistedMonadicTermStoreTestSpecsRunner
extends ConsoleRunner(PersistedMonadicTermStoreTestSpecs)


object PersistedMonadicTermStoreTestSpecs extends Specification {
  import PersistedMonadicTS._
  "basic get" should {
    "Retrieve values from the GraphFour DB" in {
      BX.reportGraphs
      val pimgJunq = ptToPt( "GraphFour", "localhost", "localhost" )
      val atps = pimgJunq.agentTwistedPairs
      val oge = BX.outerGraphExprCCL

      var eVal = ""

      reset {
	for( e <- pimgJunq.get( oge ) )
	  {
	    println( "received: " + e )
	    e match {
	      case Some(
		mTT.RBound(
		  Some( mTT.Ground( graphSpec ) ),
		  None
		)
	      ) => {
		eVal = e.toString
	      }
	      case _ => {
		throw new Exception(
		  "received unexpect value from	test " + e
		)
	      }
	    }
	  }
      }

      while ( eVal == "" ) {
	Thread.sleep( 100 )
      }

      eVal.length must be >= 0
      eVal.indexOf("Connected") must be >= 0
    }
  }
}
