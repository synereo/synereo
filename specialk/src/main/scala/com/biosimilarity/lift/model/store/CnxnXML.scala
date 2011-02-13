// -*- mode: Scala;-*- 
// Filename:    CnxnXML.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 10 03:57:46 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.store.zipper._

import scala.xml._
import scala.util.parsing.combinator._

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

trait CnxnXML[Namespace,Var,Tag] {
  self : CnxnCtxtInjector[Namespace,Var,Tag] with UUIDOps =>

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

trait CnxnXQuery[Namespace,Var,Tag] {
  self : CnxnCtxtInjector[Namespace,Var,Tag]
	  with UUIDOps with CnxnXML[Namespace,Var,Tag] =>
	    // Indentation
  
  trait TermIndex
  case class DeBruijnIndex(
    depth : int,
    width : int
  ) extends TermIndex

  trait XQueryCompilerContext {
    def root : CnxnCtxtBranch[Namespace,Var,Tag]
    def xqVar : String
    def parent : XQueryCompilerContext
    def location : Location[Either[Tag,Var]]
    def index : DeBruijnIndex
  }
  case class XQCC( 
    override root : CnxnCtxtBranch[Namespace,Var,Tag],
    override xqVar : String,
    override parent : XQueryCompilerContext
    override location : Location[Either[Tag,Var]]
    override index : DeBruijnIndex
  ) extends XQueryCompilerContext

  def asXQueryString(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
  ) : String = {
    cnxn match {
      case leaf : CnxnCtxtLeaf[Namespace,Var,Tag] =>
	asXQueryStr( leaf, xqcc )
      case branch : CnxnCtxtBranch[Namespace,Var,Tag] =>
	asXQueryStr( branch, xqcc )
    }
  }

  def asXQueryStr(
    leaf : CnxnCtxtLeaf[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
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

  def createVariableName( root : String ) : String = {
    "$" + root + "_" + getUUID 
  }

  def xQueryConstraint(
    leafTag : Tag,
    xqcc : XQueryCompilerContext,
    index : DeBruijnIndex
  ) : String = {
    val cVar = createVariableName( "" )
    val lHS =
      (
	xqcc.xqVar
	+ "/"
	+ cVar + "_" + index.depth + "_" + index.width
      )
    val rHS = "" + leafTag
    lHS + "=" + rHS
  }

  def xQueryIter(
    leafVar : Var,
    xqcc : XQueryCompilerContext
  ) : String = {
    val iVar = createVariableName( leafVar + "" )
    val lHS = "" + leafTag
    val rHS =
      (
	xqcc.xqVar
	+ "/"
	+ iVar + "_" + index.depth + "_" + index.width
      )    
    lHS + "=" + rHS
  }

  def asXQueryStr(
    branch : CnxnCtxtBranch[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
  ) : String = {
    val tagStr = branch.nameSpace
    val elemItrV = createVariableName( tagStr )
    val ( constraints, varsNSubterms ) =
      branch.factuals.partition( 
	{ 
	  ( fact ) => {
	    fact match {
	      case CnxnCtxtLeaf( Left( t ) ) => true
	      case _ => false
	    }
	  }		 
       }
      )
    val ( vars, subterms ) =
      varsNSubterms.partition(
	( fact ) => match {
	  fact match {
	    case CnxnCtxtLeaf( Right( v ) ) => true
	    case _ => false
	  }
	}
      )

    val sxqcc = XQCC( branch, elemItrV, xqcc )

    val constraintExprs = 
      constraints match {
	case constraint :: rconstraints => {
	  ( xQueryConstraint( constraint, sxqcc ) /: rconstraints )(
	    {
	      ( acc, cnstrnt ) => {
		acc + " & " + xQueryConstraint( cnstrnt, sxqcc )
	      }
	    }
	  )
	}
	case Nil => ""
      }    

    val iterExprs =
      vars match {
	case v :: rvs => {
	  ( xQueryIter( v, sxqcc ) /: rvs )(
	    {
	      ( acc, v ) => {
		acc + " & " + xQueryIter( v, sxqcc )
	      }
	    }
	  )
	}
	case Nil => ""
      }
	
    val branchExpr = ""

    val xQueryNode = 
      <xqueryExpr>for {elemItrV} in {tagStr} where {constraintExprs} return {branchExpr}<xqueryExpr>
    
    xQueryNode.Text
  }
}

