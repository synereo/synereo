// -*- mode: Scala;-*- 
// Filename:    NodeAdapter.scala 
// Authors:     lgm                                                    
// Creation:    Tue Oct 25 15:39:51 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.node

import com.biosimilarity.lift.lib.monad._
import com.biosimilarity.lift.lib.moniker._

import com.biosimilarity.lift.lib.AMQPDefaults
import com.biosimilarity.lift.lib.AMQPTwistedPairScope
import com.biosimilarity.lift.lib.JSONOverAMQPTwistedPairScope
import com.biosimilarity.lift.lib.AMQPStdTwistedPairScope
import com.biosimilarity.lift.lib.AMQPStdTPS
import com.biosimilarity.lift.lib.StdJSONOverAMQPTwistedPairScope
import com.biosimilarity.lift.lib.StdJSONOverAMQPTPS
import com.biosimilarity.lift.lib.AMQPBrokerScope
import com.biosimilarity.lift.lib.AMQPScope
import com.biosimilarity.lift.lib.JSONOverAMQPBrokerScope
import com.biosimilarity.lift.lib.AMQPStdScope
import com.biosimilarity.lift.lib.AMQPNodeJSScope
import com.biosimilarity.lift.lib.MonadicAMQPDispatcher
import com.biosimilarity.lift.lib.JSONWireToTrgtConversion

import com.biosimilarity.lift.lib.json._
import com.biosimilarity.lift.lib.json.Absyn._

import com.biosimilarity.lift.lib.parsing.prolog.term._

import com.biosimilarity.lift.model.store.CnxnLabel
import com.biosimilarity.lift.model.store.OntologicalStatus
import com.biosimilarity.lift.model.store.Factual
import com.biosimilarity.lift.model.store.Hypothetical
import com.biosimilarity.lift.model.store.Theoretical
import com.biosimilarity.lift.model.store.CnxnLeaf
//import com.biosimilarity.lift.model.store.CCnxnLeaf
import com.biosimilarity.lift.model.store.AbstractCnxnBranch
import com.biosimilarity.lift.model.store.CnxnBranch
//import com.biosimilarity.lift.model.store.CCnxnBranch
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.biosimilarity.lift.model.store.CnxnCtxtLeaf
//import com.biosimilarity.lift.model.store.CCnxnCtxtLeaf
import com.biosimilarity.lift.model.store.AbstractCnxnCtxtBranch
import com.biosimilarity.lift.model.store.CnxnCtxtBranch
//import com.biosimilarity.lift.model.store.CCnxnCtxtBranch
import com.biosimilarity.lift.model.store.CnxnCtxtInjector
import com.biosimilarity.lift.model.store.Cnxn
import com.biosimilarity.lift.model.store.CCnxn
import com.biosimilarity.lift.model.store.CnxnXML
import com.biosimilarity.lift.model.store.Blobify
import com.biosimilarity.lift.model.store.CnxnXQuery

import net.liftweb.amqp._

import scala.util.continuations._

import scala.concurrent.{Channel => Chan, _}
//import scala.concurrent.cpsops._
import scala.collection.immutable.HashMap
import scala.util.parsing.combinator._

import com.rabbitmq.client.{ Channel => RabbitChan, _}

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.io.IOException;
import java.util.concurrent.Future;

import java.net.URI
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.util.Timer
import java.util.TimerTask
import java.io.StringReader

class JSONRequestREPL
  //extends FJTaskRunnersX
  extends ThreadPoolRunnersX
with PrologTermParsing {
  // parsing
  def lexer (str : String) = new Yylex( new StringReader( str ) )
  def parser (str : String) = new parser( lexer( str ) )
  def clientRequestParseTree (str : String) = (parser( str )).pJSONObject()
  def read (str : String) = clientRequestParseTree(str)    

  // printing
  def showClientRequestParseTree (str : String) =
    PrettyPrinter.show(clientRequestParseTree(str))    

  def xformInput( expr : String ) : Option[CnxnCtxtLabel[String,String,Any]] = {
    xformSyntax( PrettyPrinter.show( read( expr ) ) )    
  }
  
  def exchange(
    srcHost : String, trgtHost : String,
    srcQStr : String, trgtQStr : String
  ) = {
    val srcScope = new AMQPStdScope[String]() {
      override def properties : Option[AMQP.BasicProperties] = {    
	val amqpBPBuilder = new AMQP.BasicProperties.Builder
	amqpBPBuilder.contentType( "application/json" )
	Some( amqpBPBuilder.build )
      }
    }
    val srcQM =
      new srcScope.AMQPQueueHostExchangeM[String]( srcHost, srcQStr )
    val srcQ = srcQM.zero[String]

    val trgtScope = new AMQPStdScope[String]()
    val trgtQM =
      new trgtScope.AMQPQueueHostExchangeM[String]( trgtHost, trgtQStr )
    val trgtQ = trgtQM.zero[String]
    
    spawn {
      for( msg <- srcQM( srcQ ) ) {
	println(
	  (
	    "received msg " + msg
	    + " on " + srcQStr 
	  )
	)      
      }
    }

    trgtQ ! "{application:{operation:{abstraction:{formal:\"u\",body:{mention:\"u\"}}},actual:{abstraction:{formal:\"u\",body:{mention: \"u\"}}}}}"
  }

  def exchangeNode(
    srcHost : String, trgtHost : String,
    srcQStr : String, trgtQStr : String
  ) = {
    val srcScope = new AMQPNodeJSScope( AMQPDefaults.defaultConnectionFactory ) {
      @transient override lazy val properties : Option[AMQP.BasicProperties] = {    
	val amqpBPBuilder = new AMQP.BasicProperties.Builder
	amqpBPBuilder.contentType( "text/plain" )
	Some( amqpBPBuilder.build )
      }
    }
    val srcQM =
      new srcScope.AMQPNodeJSQueueM( srcHost, srcQStr )
    val srcQ = srcQM.zeroJSON

    val trgtScope = new AMQPNodeJSScope( AMQPDefaults.defaultConnectionFactory )
    val trgtQM =
      new trgtScope.AMQPNodeJSQueueM( trgtHost, trgtQStr )
    val trgtQ = trgtQM.zeroJSON
    
    spawn {
      for( msg <- srcQM( srcQ ) ) {
	println(
	  (
	    "received msg " + msg
	    + " on " + srcQStr 
	  )
	)      
      }
    }

    trgtQ ! "{application:{operation:{abstraction:{formal:\"u\",body:{mention:\"u\"}}},actual:{abstraction:{formal:\"u\",body:{mention: \"u\"}}}}}"
  }
  
}
