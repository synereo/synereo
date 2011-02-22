// -*- mode: Scala;-*- 
// Filename:    CnxnXML.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 10 03:57:46 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.zipper._
import com.biosimilarity.lift.lib._

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
    depth : Int,
    width : Int
  ) extends TermIndex

  trait XQueryCompilerContext {
    def root : CnxnCtxtBranch[Namespace,Var,Tag]
    def xqVar : String
    def parent : Option[XQueryCompilerContext]
    def location : Option[Location[Either[Tag,Var]]]
    def index : DeBruijnIndex
  }
  case class XQCC( 
    override val root : CnxnCtxtBranch[Namespace,Var,Tag],
    override val xqVar : String,
    override val parent : Option[XQueryCompilerContext],
    override val location : Option[Location[Either[Tag,Var]]],
    override val index : DeBruijnIndex
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

  def createVariableName( uniquify : Boolean )( root : String ) : String = {
    "$" + root + (if ( uniquify ) { "_" + getUUID } else { "" })
  }

  def xQueryConstraint(
    leafTag : Tag,
    xqcc : XQueryCompilerContext,
    index : DeBruijnIndex
  ) : String = {
    val cVar = createVariableName( false )( "xqV" )
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
    xqcc : XQueryCompilerContext,
    index : DeBruijnIndex
  ) : String = {
    val iVar = createVariableName( false )( leafVar + "" )
    val lHS = iVar + "_" + index.depth + "_" + index.width
    val rHS =    
      (
	xqcc.xqVar
	+ "["
	+ index.width 
	+ "]"
      )    
      lHS + " in " + rHS
  }

  def asXQueryStr(
    branch : CnxnCtxtBranch[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
  ) : String = {
    println( "entering asXQueryStr with : " )
    println( "branch : " + branch )
    println( "xqcc : " + xqcc )

    val tagStr = branch.nameSpace + ""
    val elemItrV = createVariableName( false )( tagStr + "_Elem" )
    val sIndx = 
      DeBruijnIndex( xqcc.index.depth + 1, 0 )
    val placeStr =
      (	
	xqcc.xqVar + "/" + tagStr
      )      

    val sxqcc =
      XQCC(
	branch,
	elemItrV,
	Some( xqcc ),
	None,
	sIndx
      )

    val ndentStr =
      ( "" /: (0 to xqcc.index.depth) )(
	{ ( acc, s ) => acc + " " }
      )

    val ( cclConstraints, varsNSubterms ) =
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
    println( "cclConstraints : " + cclConstraints )
    val constraints =
      cclConstraints.map( { case CnxnCtxtLeaf( Left( t ) ) => t } )
    println( "constraints : " + constraints )

    val ( cclVars, subterms ) =
      varsNSubterms.partition(
	( fact ) => {
	  fact match {
	    case CnxnCtxtLeaf( Right( v ) ) => true
	    case _ => false
	  }
	}
      )
    println( "cclVars : " + cclVars )
    val vars = cclVars.map( { case CnxnCtxtLeaf( Right( v ) ) => v } )
    println( "vars : " + vars )    

    println( "calculating constraint expressions " )
    val ( constraintExprs, _ ) = 
      constraints match {
	case constraint :: rconstraints => {
	  val seedStr = 
	    (
	      " " + "where" + " "
	      + xQueryConstraint(
		constraint,
		sxqcc,
		sIndx
	      )
	    );
	    
	  ( ( seedStr, sIndx ) /: rconstraints )(
	    {
	      ( acc, cnstrnt ) => {
		val ( accStr, DeBruijnIndex( d, w ) ) = acc
		val ndbIdx = DeBruijnIndex( d, w + 1 )
		val nSeedStr = 
		  (
		    accStr
		    + " & "
		    + xQueryConstraint(
		      cnstrnt, sxqcc, ndbIdx
		    )
		  )
		( nSeedStr, ndbIdx )
	      }
	    }
	  )
	}
	case Nil => ( "", sIndx )
      }

    println( "constraint expressions : " + constraintExprs )

    println( "calculating iter expressions " )
    val ( iterExprsL, _ ) =
      vars match {
	case v :: rvs => {
	  val seedStr = 
	    (
	      "\n"
	      + ndentStr
	      + "for" + " "
	      + xQueryIter( v, sxqcc, sIndx )
	    );

	  ( ( seedStr, sIndx ) /: rvs )(
	    {
	      ( acc, v ) => {
		val ( accStr, DeBruijnIndex( d, w ) ) = acc
		val ndbIdx = DeBruijnIndex( d, w + 1 )
		val nSeedStr = 
		  (
		    accStr + ",\n"
		    + ndentStr + "    "
		    + xQueryIter( v, sxqcc, ndbIdx )
		  )
		
		( nSeedStr, ndbIdx )
	      }
	    }
	  )
	}

	case Nil => ( "", sIndx )
      }

    val iterExprs = 
      iterExprsL match {
	case "" => ""
	case _ => iterExprsL + " return\n"
      }

    val elemVars =
      iterExprs
      .replaceAll( " in [^\\]]*\\]", "" )
      .replace( "for ", "" )
      .replaceAll( "\n", "" )
      .replaceAll( " *", "" )

    println( "iter expressions : " + iterExprs )

    println( "calculating subterm expressions " )
    val ( subtermExprs, _ ) =
      subterms match {
	case st :: rst => {
	  val seedStr =
	    (
	      "\n" + ndentStr
	      + asXQueryString( st, sxqcc )
	    );
	    
	  ( ( seedStr, sIndx ) /: rst )(
	    {
	      ( acc, st ) => {
		val ( accStr, DeBruijnIndex( d, w ) ) = acc
		val ndbIdx = DeBruijnIndex( d, w + 1 )
		val nSeedStr = 
		  accStr + "\n" + ndentStr + asXQueryString( st, sxqcc )

		( nSeedStr, ndbIdx )
	      }
	    }
	  )
	}
	case Nil => ( "", sIndx )
      }
    println( "subterm expressions : " + subtermExprs )
	
    val branchExpr =
      elemVars match {
	case "" =>
	  subtermExprs
	case _ =>
	  subtermExprs match {
	    case "" =>
	      elemVars
	    case _ =>
	      elemVars + "\n" + subtermExprs + "\n"
	  }	
      }    

    val ctorExpr = 
      branchExpr match {
	case "" =>
	  (
	    xqcc.xqVar + "/" + elemItrV + "\n"
	  )
	case _ =>
	  xmlTrampoline( tagStr, "{" + branchExpr + "}" ).toString
      }
    
    println( "ctor expression : " + ctorExpr )

    val xQueryNode = 
      <xqueryExpr>for {elemItrV} in {placeStr}{constraintExprs} return {iterExprs}</xqueryExpr>
    xQueryNode.text + "\n" + ndentStr + ctorExpr.replace( "&amp;", "&" )
  }
}


