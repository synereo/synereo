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

  def asCCIString [A,B,C]( ccl : CnxnCtxtLabel[A,B,C] ) : String = {
    ccl match {
      case CnxnCtxtLeaf( Left( t ) ) => t + ""
      case CnxnCtxtLeaf( Right( v ) ) => "'" + v
      case ccb : CnxnCtxtBranch[A,B,C] => ccb.flatMap(
	{
	  ( x ) => {
	    x match {
	      case Left( Left( t ) ) => List( t + "" )
	      case Left( Right( v ) ) => List( "'" + v )
	      case Right( f : CnxnCtxtLabel[A,B,C] ) => {
		List( asCCIString( f ) )
	      }
	    }
	  }
	}
      ).toString.replaceFirst( "List", (ccb.nameSpace + "") )
    }
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
    
    def varCountSeed = 0
    def varRoot = "xqV"
  
    def createVariableName( uniquify : Boolean )(
      root : String
    ) : String = {
      "$" + varRoot + root + (if ( uniquify ) { "_" + getUUID } else { "" })
    }

    def varStream( n : Int ) : Stream[String] = {
      lazy val vStrm : Stream[Int] =
	List( n ).toStream append vStrm.map( _ + 1 )
      vStrm.map( createVariableName( false )( "" ) + _ )
    }  

    var _xqVarStrm : Option[Stream[String]] = None
    def nextXQVars( n : Int ) : Stream[String] = {
      val vstrm = _xqVarStrm.getOrElse( varStream( varCountSeed ) )
      val rslt = vstrm.take( n )
      _xqVarStrm = Some( vstrm.drop( n ) )
      rslt
    }
    def nextXQV = {
      nextXQVars( 1 )( 0 )
    }
  }

  case class XQCC( 
    override val root : CnxnCtxtBranch[Namespace,Var,Tag],
    override val xqVar : String,
    override val parent : Option[XQueryCompilerContext],
    override val location : Option[Location[Either[Tag,Var]]],
    override val index : DeBruijnIndex
  ) extends XQueryCompilerContext

  def xqForPath(
    xqcc : XQueryCompilerContext,
    vNS : String
  ) : String = {
    xqcc.xqVar + "/" + vNS
  }  

  def arity(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
  ) : Int = {
    ccl match {
      case CnxnCtxtLeaf( Left( t ) ) => 0
      case CnxnCtxtLeaf( Right( v ) ) => 0
      case CnxnCtxtBranch( ns, fs ) => fs.length
    }
  }

  def xqConstraints(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
  ) : List[String] = {
    def cPaths(
      ccl : CnxnCtxtLabel[Namespace,Var,Tag],
      idx : Int
    ) : List[String] = {
      ccl match {
	case CnxnCtxtLeaf( Left( t ) ) =>
	  List( "*" + "[" + idx + "]" + " = " + t + "" )
	case CnxnCtxtLeaf( Right( v ) ) =>
	  List( "" )
	case CnxnCtxtBranch( ns, fs ) => {
	  //fs.flatMap( cPaths( _ ) ).map( ns + "/" + _ )
	  val ( paths, _ ) =
	    ( ( ( Nil : List[String] ), 0 ) /: fs )(
	      {
		( acc, f ) => {
		  val ( ps, i ) = acc
		  ( ps ++ cPaths( f, i ).map( ns + "/" + _ ), i+1 )
		}
	      }
	    )
	  paths
	}
      }
    }
    cPaths( ccl, 0 ) 
  }
  
  def xqWhereClause(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
  ) : Option[String] = {
    val xqCnsts = xqConstraints( ccl, xqcc )
    val arityConstraint = 
      ccl match {
	case CnxnCtxtLeaf( _ ) => ""
	case CnxnCtxtBranch( ns, fs ) =>
	  (
	    " " + "and" + " "
	    + "count" + "("
	    + xqForPath( xqcc, ns + "" ) + "/" + "*"
	    + ")"
	    + " " + "=" + " " + fs.length + " "
	  )
      }

    xqCnsts match {
      case xqc :: xqcs => {
	val nxqc =
	  xqCnsts.map(
	    ( p ) => {
	      if ( p.lastIndexOf( "/" ) ==  p.length - 1 ) {
		(
		  "{" + " "
		  + "some" + " " 
		  + xqcc.nextXQV + " " + "in" + " "
		  + p + "*" + " "
		  + "satisfies" + " " + "true"
		  + " " + "}"
		)
	      }
	      else {
		p
	      }
	    }
	  )

	Some(
	  nxqc.toString
	  .replace( "List", "where" )
	  .replace( ",", " and" )
	  .replace( "(", " " )
	  .replace( ")", "" )
	  .replace( "{", "(" )
	  .replace( "}", ")" )
	  + arityConstraint
	)
      }
      case Nil => {
	arityConstraint match {
	  case "" => None
	  case _ => Some( arityConstraint )
	}	
      }
    }    
  }

  def xqForClause(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
  ) : Option[(String,String)] = {
    val nxqv = xqcc.nextXQV
    val forStr =
      (
	"for" + " " + nxqv + " " + "in" + " " 
      )
	
    ccl match {
      case ccb : CnxnCtxtBranch[Namespace,Var,Tag] => {	
	Some(
	  ( nxqv, forStr + xqForPath( xqcc, ccb.nameSpace + "" ) )
	)
      }
      case CnxnCtxtLeaf( _ ) => {	
	Some( ( nxqv, forStr + "*" ) )
      }
    }    
  }
  
  def xqReturnClause(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    nxqv : String,
    xqcc : XQueryCompilerContext
  ) : Option[String] = {
    Some(
      "return" + " " + nxqv
    )
  }

  def xqQuery(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
  ) : Option[String] = {
    xqForClause( ccl, xqcc ) match {
      case Some( ( nxqv, xq4cl ) ) => {
	val xqwcl = xqWhereClause( ccl, xqcc ).getOrElse( "" )
	val Some( xqretcl ) = xqReturnClause( ccl, nxqv, xqcc )
	
	Some( xq4cl + " " + xqwcl + xqretcl )
      }
      case None => None
    }    
  }
  
  def xqQuery(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : String = {
    ccl match {
      case CnxnCtxtLeaf( t ) => ""
      case ccb : CnxnCtxtBranch[Namespace,Var,Tag] => {
	val xqcc =
	  XQCC( ccb, "root", None, None, DeBruijnIndex( 0, 0 ) )
	xqQuery( ccl, xqcc ).getOrElse( "" )
      }
    }     
  }
}


