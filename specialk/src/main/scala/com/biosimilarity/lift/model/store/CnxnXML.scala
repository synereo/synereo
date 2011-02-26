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

  trait XQueryCompilerContext {
    def root : CnxnCtxtBranch[Namespace,Var,Tag]
    def branch : CnxnCtxtLabel[Namespace,Var,Tag]
    def xqVar : String
    def parent : Option[XQueryCompilerContext]
    def location : Option[Location[Either[Tag,Var]]]
    def index : DeBruijnIndex
    
    def sibling(
      branch : CnxnCtxtLabel[Namespace,Var,Tag],
      nxqv : String
    ) : XQueryCompilerContext

    def child(
      branch : CnxnCtxtLabel[Namespace,Var,Tag],
      nxqv : String
    ) : XQueryCompilerContext

    def child(
      branch : CnxnCtxtLabel[Namespace,Var,Tag],
      nxqv : String,
      width : Int
    ) : XQueryCompilerContext
  }

  case class XQCC( 
    override val root : CnxnCtxtBranch[Namespace,Var,Tag],
    override val branch : CnxnCtxtLabel[Namespace,Var,Tag],
    override val xqVar : String,
    override val parent : Option[XQueryCompilerContext],
    override val location : Option[Location[Either[Tag,Var]]],
    override val index : DeBruijnIndex
  ) extends XQueryCompilerContext {
    override def sibling(
      branch : CnxnCtxtLabel[Namespace,Var,Tag],
      nxqv : String
    ) =
      XQCC(
	root, branch, nxqv, parent, location,
	DeBruijnIndex( index.depth, index.width + 1 )
      )
    override def child(
      branch : CnxnCtxtLabel[Namespace,Var,Tag],
      nxqv : String
    ) =
      XQCC(
	root, branch, nxqv, Some( this ), location, 
	DeBruijnIndex( index.depth + 1, 0 )
      )
    override def child(
      branch : CnxnCtxtLabel[Namespace,Var,Tag],
      nxqv : String,
      width : Int
    ) =
      XQCC(
	root, branch, nxqv, Some( this ), location, 
	DeBruijnIndex( index.depth + 1, width )
      )
  }

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

  def xqRecConstraints(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
  ) : String = {

    val width = xqcc.index.width
    val depth = xqcc.index.depth
    val xqVar = xqcc.xqVar

    println( "entering xqRecConstraints with " )
    println( "xqVar : " + xqVar )
    println( "width : " + width )
    println( "depth : " + depth )

    ccl match {
      case CnxnCtxtLeaf( Left( t ) ) =>
	( xqVar + "/" + "*" + "[" + width + "]" + " = " + t + "" )
      case CnxnCtxtLeaf( Right( v ) ) =>
	( "" )
      case CnxnCtxtBranch( ns, facts ) => {
	val nxqv = nextXQV
	println( "generated next var: " + nxqv )

	val ( childConstraints, _ ) =
	  facts match {
	    case fact :: fs => {
	      val fxqcc = xqcc.child( fact, nxqv, 0 )
	      val factC = xqRecConstraints( fact, fxqcc )
	      ( ( factC, 1 ) /: fs )( 
		{
		  ( acc, f ) => {
		    val ( ccs, w ) = acc
		    val nxqcc = xqcc.child( f, nxqv, w )
		    val fC = xqRecConstraints( f, nxqcc )
		    (
		      (fC match {
			case "" => ccs 
			case _ => ccs + " and " + fC 
		      }),
		      w+1
		    )
		  }
		}
	      )
	    }
	    case Nil => ( "", 0 )
	  }	

	val arityC = (
	  "count" + "(" + nxqv + "/" + "*" + ")" 
	  + " = " + facts.length
	)

	val existentialC = (
	  "some" + " " + nxqv + " in " + xqVar + "/" + ns
	  + " satisfies "
	  + (
	    childConstraints match {
	      case "" =>
		arityC
	      case _ =>
		(
		  "(" + childConstraints + ")"
		  + " and "
		  + arityC + " "
		)
	    } 
	  )	  
	)		  
	
        existentialC
      }
    }
  }

  def xqConstraints(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    nxqv : String,
    xqcc : XQueryCompilerContext
  ) : List[String] = {
    val cntMark = getUUID.toString
    def cPaths(
      ccl : CnxnCtxtLabel[Namespace,Var,Tag],
      depth : Int,
      width : Int
    ) : List[String] = {
      ccl match {
	case CnxnCtxtLeaf( Left( t ) ) =>
	  List( "*" + "[" + width + "]" + " = " + t + "" )
	case CnxnCtxtLeaf( Right( v ) ) =>
	  List( "" )
	case CnxnCtxtBranch( ns, fs ) => {
	  val prfx =
	    depth match {
	      case 0 => nxqv
	      case _ => ns
	    }
	  val ( paths, _ ) =
	    ( ( ( Nil : List[String] ), 0 ) /: fs )(
	      {
		( acc, f ) => {
		  val ( ps, i ) = acc
		  (
		    ps ++ cPaths( f, depth+1, i ).map(
		      {
			( path ) => {
			  if ( path.indexOf( cntMark ) == 0 ) {
			    //println( "in count branch with path: " + path )
			    val npath =
			      path.replace(
				cntMark + "{", cntMark + "{" + prfx + "/" 
			      )
			    //println( "leaving count branch with new path: " + npath )
			    npath
			  }
			  else {
			    //println( "in std branch with path: " + path )
			    val npath = prfx + "/" + path
			    //println( "leaving std branch with new path: " + npath )
			    npath
			  }
			}
		      }
		    ),
		   i+1
		  )
		}
	      }
	    )
	  val arityC =
	    (
	      cntMark + "{" + prfx + "/" + "*" + "}" + " = " + fs.length
	    )
	  
	  val rslt = paths ++ List( arityC )
	  //println( "leaving cpaths with: " + rslt )
	  rslt
	}
      }
    }

    cPaths( ccl, 0, 0 ).map( _.replace( cntMark, "count" ) )
  }
  
  def xqWhereClause(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    nxqv : String,
    xqcc : XQueryCompilerContext
  ) : Option[String] = {
    val xqCnsts = xqConstraints( ccl, nxqv, xqcc )    

    xqCnsts match {
      case xqc :: xqcs => {
	val nxqc =
	  xqCnsts.filter(
	    ( p ) => {
	       ( p.lastIndexOf( "/" ) !=  p.length - 1 )
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
	  + " "
	)
      }
      case Nil => None
    }    
  }

  def xqForClause(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
  ) : Option[(String,String)] = {
    val nxqv = nextXQV
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
	val xqwcl = xqWhereClause( ccl, nxqv, xqcc ).getOrElse( "" )
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
	  XQCC( ccb, ccb, "root", None, None, DeBruijnIndex( 0, 0 ) )
	xqQuery( ccl, xqcc ).getOrElse( "" )
      }
    }     
  }
}


