// -*- mode: Scala;-*- 
// Filename:    Milner.scala 
// Authors:     lgm                                                    
// Creation:    Mon Oct 24 12:11:41 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.rlambda.evaluator

import com.biosimilarity.rlambdaDC.lang.rlambdaDC._
import Absyn._

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.util.continuations._ 
import scala.util.parsing.combinator._
import scala.xml._
import scala.collection.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MutableList

import org.prolog4j._

//import org.exist.storage.DBBroker

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
import java.io.File
import java.io.FileInputStream
import java.io.OutputStreamWriter
import java.util.UUID
import java.io.StringReader


trait EvaluateTerm {
  def lexer (str : String) = new Yylex( new StringReader( str ) )
  def parser (str : String) = new parser( lexer( str ) )
  def clientRequestParseTree (str : String) = (parser( str )).pExpression()
  def read (str : String) = clientRequestParseTree(str)  

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
}
