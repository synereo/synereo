// -*- mode: Scala;-*- 
// Filename:    CnxnXML.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 10 03:57:46 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import scala.xml._
import scala.util.parsing.combinator._

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

trait CnxnXML[Namespace,Var,Tag] {
  self : CnxnCtxtInjector[Namespace,Var,Tag] =>

  def toXML( cnxn : CnxnLabel[Namespace,Tag] ) : String = {
    toXML( injectLabel( cnxn ) )
  }
  def toXML( cnxn : CnxnCtxtLabel[Namespace,Var,Tag] ) : String = {
    new XStream( ).toXML( cnxn )
  }
  def asXML( cnxn : CnxnCtxtLabel[Namespace,Var,Tag] ) : Node = {
    cnxn match {
      case leaf : CnxnCtxtLeaf[Namespace,Var,Tag] =>
	asXMLData( leaf )
      case branch : CnxnCtxtBranch[Namespace,Var,Tag] =>
	asXMLData( branch )
    }
  }

  def xmlTrampoline( tagStr : String, v : String ) : Elem = {
    val trampoline = 	  
      <trampoline>{ "<" + tagStr + ">" + v + "</" + tagStr + ">" }</trampoline>
    XML.loadString(
      trampoline
      .child( 0 )
      .toString
      .replace( "&lt;", "<" )
      .replace( "&gt;", ">" )
    )
  }

  def asXMLData(
    leaf : CnxnCtxtLeaf[Namespace,Var,Tag]
  ) : Node = {
    leaf.tag match {
      case Left( t ) => {
	val tagStr =
	  t.asInstanceOf[Object]
	.getClass.toString
	.replace( "class ", "")
	.replace( "java.lang", "")
	.replace( ".", "" )
	xmlTrampoline( tagStr, t.toString )
      }
      case Right( v ) => {
	<var>{ v.toString }</var>
      }
    }
  }

  def asXMLData(
    branch : CnxnCtxtBranch[Namespace,Var,Tag]
  ) : Node = {
    val tagStr = branch.nameSpace
    val fx = 
      branch.factuals match {
	case fact :: rfacts => {
	  ( asXML( fact ).toString /: rfacts )(
	    {
	      ( acc, f ) => {
		acc + asXML( f ).toString
	      }
	    }
	  )
	}
	case Nil => {
	  ""
	}
      }

    xmlTrampoline( tagStr + "", fx )    
  }

  def asCaseClassInstance(
    cnxn : CnxnLabel[Namespace,Tag]
  ) : Elem = {
    <caseClassInstance>{asCaseClassInstanceString( cnxn )}</caseClassInstance>
  }
  def asCaseClassInstanceString(
    cnxn : CnxnLabel[Namespace,Tag]
  ) : String = {
    cnxn match {
      case leaf : CnxnLeaf[Namespace,Tag] =>
	asCaseClassInstanceStr( leaf )
      case branch : CnxnBranch[Namespace,Tag] =>
	asCaseClassInstanceStr( branch )
    }
  }
  def asCaseClassInstanceStr(
    leaf : CnxnLeaf[Namespace,Tag]
  ) : String = {
    leaf.tag.toString
  }

  def asCaseClassInstanceStr(
    branch : CnxnBranch[Namespace,Tag]
  ) : String = {
    val tagStr = branch.nameSpace
    val fx = 
      branch.factuals match {
	case fact :: rfacts => {
	  ( asCaseClassInstanceString( fact ) /: rfacts )(
	    {
	      ( acc, f ) => {
		acc + "," + asCaseClassInstanceString( f ).toString
	      }
	    }
	  )
	}
	case Nil => {
	  ""
	}
      }

    tagStr + "(" + fx + ")"
  }

  def asPattern(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : Elem = {
    <pattern>{asPatternString( cnxn )}</pattern>
  }
  def asPatternString(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : String = {
    cnxn match {
      case leaf : CnxnCtxtLeaf[Namespace,Var,Tag] =>
	asPatternStr( leaf )
      case branch : CnxnCtxtBranch[Namespace,Var,Tag] =>
	asPatternStr( branch )
    }
  }
  def asPatternStr(
    leaf : CnxnCtxtLeaf[Namespace,Var,Tag]
  ) : String = {
    leaf.tag match {
      case Left( t ) => {
	t.toString
      }
      case Right( v ) => {
	v.toString
      }
    }
  }

  def asPatternStr(
    branch : CnxnCtxtBranch[Namespace,Var,Tag]
  ) : String = {
    val tagStr = branch.nameSpace
    val fx = 
      branch.factuals match {
	case fact :: rfacts => {
	  ( asPatternString( fact ) /: rfacts )(
	    {
	      ( acc, f ) => {
		acc + "," + asPatternString( f )
	      }
	    }
	  )
	}
	case Nil => {
	  ""
	}
      }

    tagStr + "(" + fx + ")"
  }

  class TermParser extends JavaTokenParsers {
    def term : Parser[Any] =
      application | ground | variable
    def ground : Parser[Any] =
      stringLiteral | floatingPointNumber | "true" | "false"
    def variable : Parser[Any] = ident
    def application : Parser[Any] =
      ident~"("~repsep( term, "," )~")"

    def termXform : Parser[CnxnCtxtLabel[String,String,Any] with Factual] =
      applicationXform | groundXform | variableXform
    def groundXform : Parser[CnxnCtxtLabel[String,String,Any] with Factual] =
      (
	stringLiteral ^^ ( x => new CnxnCtxtLeaf[String,String,Any]( Left[Any,String]( x ) ) )
	| floatingPointNumber ^^ ( x => new CnxnCtxtLeaf[String,String,Any]( Left[Any,String]( x.toDouble ) ) )
	| "true" ^^ ( x => new CnxnCtxtLeaf[String,String,Any]( Left[Any,String]( true ) ) )
	| "false" ^^ ( x => new CnxnCtxtLeaf[String,String,Any]( Left[Any,String]( false ) ) )
      )
    def variableXform : Parser[CnxnCtxtLabel[String,String,Any] with Factual] =
      ident ^^ ( x => new CnxnCtxtLeaf[String,String,Any]( Right( x.toString ) ) )
    def applicationXform : Parser[CnxnCtxtLabel[String,String,Any] with Factual] =
      ident~"("~repsep( termXform, "," )~")" ^^ {
	case ident~"("~terms~")" => new CnxnCtxtBranch[String,String,Any]( ident, terms )
      }
  }

  def fromCaseClassInstanceString(
    cciElem : String
  ) : Option[CnxnCtxtLabel[String,String,Any]] = {
    val readBack = new TermParser
    val ptree =
      readBack.parseAll(
	readBack.termXform,
	new java.io.StringReader( cciElem )
      )
    ptree match {
      case readBack.Success( r, _ ) => Some( r )
      case _ => None
    }
  }

  def fromCaseClassInstanceNode(
    cciElem : Elem
  ) : Option[CnxnCtxtLabel[String,String,Any]] = {    
    cciElem match {
      case <caseClassInstance>{exprText}</caseClassInstance> => {
	fromCaseClassInstanceString( exprText.toString )
      }
      case <pattern>{exprText}</pattern> => {
	fromCaseClassInstanceString( exprText.toString )
      }
      case _ => {
	throw new Exception( "unexpected node" )
      }
    }
  }
}

