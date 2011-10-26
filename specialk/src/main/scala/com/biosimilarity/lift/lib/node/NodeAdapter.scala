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
import com.biosimilarity.lift.lib.MonadicAMQPDispatcher
import com.biosimilarity.lift.lib.JSONWireToTrgtConversion

import com.biosimilarity.lift.lib.json._
import com.biosimilarity.lift.lib.json.Absyn._

import com.biosimilarity.lift.model.store.CnxnLabel
import com.biosimilarity.lift.model.store.OntologicalStatus
import com.biosimilarity.lift.model.store.Factual
import com.biosimilarity.lift.model.store.Hypothetical
import com.biosimilarity.lift.model.store.Theoretical
import com.biosimilarity.lift.model.store.CnxnLeaf
import com.biosimilarity.lift.model.store.CCnxnLeaf
import com.biosimilarity.lift.model.store.AbstractCnxnBranch
import com.biosimilarity.lift.model.store.CnxnBranch
import com.biosimilarity.lift.model.store.CCnxnBranch
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.biosimilarity.lift.model.store.CnxnCtxtLeaf
import com.biosimilarity.lift.model.store.CCnxnCtxtLeaf
import com.biosimilarity.lift.model.store.AbstractCnxnCtxtBranch
import com.biosimilarity.lift.model.store.CnxnCtxtBranch
import com.biosimilarity.lift.model.store.CCnxnCtxtBranch
import com.biosimilarity.lift.model.store.CnxnCtxtInjector
import com.biosimilarity.lift.model.store.Cnxn
import com.biosimilarity.lift.model.store.CCnxn
import com.biosimilarity.lift.model.store.CnxnXML
import com.biosimilarity.lift.model.store.Blobify
import com.biosimilarity.lift.model.store.CnxnXQuery

import net.liftweb.amqp._

import scala.util.continuations._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
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

class JSONRequestREPL extends FJTaskRunners {
  // parsing
  def lexer (str : String) = new Yylex( new StringReader( str ) )
  def parser (str : String) = new parser( lexer( str ) )
  def clientRequestParseTree (str : String) = (parser( str )).pJSONObject()
  def read (str : String) = clientRequestParseTree(str)    

  // printing
  def showClientRequestParseTree (str : String) =
    PrettyPrinter.show(clientRequestParseTree(str))    

  // Concrete syntax to abstract syntax xform
  class CTermParser extends JavaTokenParsers {
    def term : Parser[Any] =
      application | list | atom | ground | variable
    def list : Parser[Any] =
      "["~repsep( term, "," )~"]"
    def atom : Parser[Any] = "'"~ident
    def ground : Parser[Any] =
      stringLiteral | floatingPointNumber | "true" | "false"
    def variable : Parser[Any] = ident
    def application : Parser[Any] =
      "("~ident~rep( term )~")"
    
    def termXform : Parser[CnxnCtxtLabel[String,String,Any] with Factual] =
      applicationXform | listXform | atomXform | groundXform | variableXform
    def listXform : Parser[CnxnCtxtLabel[String,String,Any] with Factual] = 
      "["~repsep( termXform, "," )~"]" ^^ {
	case "["~terms~"]" => {
	  new CnxnCtxtBranch[String,String,Any]( "list", terms )
	}
      }
    def atomXform : Parser[CnxnCtxtLabel[String,String,Any] with Factual] = 
      "'"~ident ^^ ( x => new CnxnCtxtLeaf[String,String,Any]( Left[Any,String]( x.toString.replace( "~", "" ) ) ) )
    def groundXform : Parser[CnxnCtxtLabel[String,String,Any] with Factual] =
      (
	stringLiteral ^^ ( x => new CnxnCtxtLeaf[String,String,Any]( Left[Any,String]( x.replace( "\"", "" ) ) ) )
	| floatingPointNumber ^^ ( x => new CnxnCtxtLeaf[String,String,Any]( Left[Any,String]( x.toDouble ) ) )
	| "true" ^^ ( x => new CnxnCtxtLeaf[String,String,Any]( Left[Any,String]( true ) ) )
	| "false" ^^ ( x => new CnxnCtxtLeaf[String,String,Any]( Left[Any,String]( false ) ) )
      )
      def variableXform : Parser[CnxnCtxtLabel[String,String,Any] with Factual] =
	ident ^^ ( x => new CnxnCtxtLeaf[String,String,Any]( Right( x.toString ) ) )
    def applicationXform : Parser[CnxnCtxtLabel[String,String,Any] with Factual] =
      "("~ident~rep( termXform )~")" ^^ {
	case "("~ident~terms~")" => new CnxnCtxtBranch[String,String,Any]( ident, terms )
      }
  }

  def xformSyntax( xpr : String ) :
  Option[CnxnCtxtLabel[String,String,Any]] = {
    val readBack = new CTermParser
    val ptree =
      readBack.parseAll(
	readBack.termXform,
	new java.io.StringReader( xpr )
      )
    ptree match {
      case readBack.Success( r, _ ) => Some( r )
      case _ => None
    }
  }
  
  def xformInput( expr : String ) : Option[CnxnCtxtLabel[String,String,Any]] = {
    xformSyntax( PrettyPrinter.show( read( expr ) ) )    
  }
  
  def exchange(
    srcHost : String, trgtHost : String,
    srcQStr : String, trgtQStr : String
  ) = {
    val srcScope = new AMQPStdScope[String]()
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
  
}
