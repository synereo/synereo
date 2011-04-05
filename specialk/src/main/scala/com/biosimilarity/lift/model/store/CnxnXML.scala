// -*- mode: Scala;-*- 
// Filename:    CnxnXML.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 10 03:57:46 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.zipper._

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

	val tStr = 
	  t match {
	    case s : String => s
	    case _ => t.toString
	  }

	xmlTrampoline( tagStr, tStr )
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
	stringLiteral ^^ ( x => new CnxnCtxtLeaf[String,String,Any]( Left[Any,String]( x.replace( "\"", "" ) ) ) )
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

  val javaBuiltins =
    (new java.lang.Object()).getClass.getMethods.toList.map( _.getName )

  def isGroundValueType(
    value : {def getClass() : java.lang.Class[_]}
  ) : Boolean = {
    ((value.isInstanceOf[Boolean]) 
     || (value.isInstanceOf[Int]) 
     || (value.isInstanceOf[Float])
     || (value.isInstanceOf[String])
     // put more ground types here
   )
  }

  def fromCaseClass(
    cc : Product
  ) : CnxnCtxtLabel[String,String,String] with Factual = {    
    def fromCC(
      cc : java.lang.Object
    ) : CnxnCtxtLabel[String,String,String] with Factual = {
      if ( isGroundValueType( cc ) ) {
	new CnxnCtxtLeaf[String,String,String](
	  Left( cc.toString )
	)
      }
      else {
	val facts =
	  (for(
	    m <- cc.getClass.getMethods;      
	    if ((! javaBuiltins.contains( m.getName ) )
		&& (( m.getParameterTypes.size ) == 0)
		&& (!java.util.regex.Pattern.matches( "product.*" , m.getName ))
		&& (!java.util.regex.Pattern.matches( "copy.default.*" , m.getName ))
	      )
	  ) yield {
	    new CnxnCtxtBranch[String,String,String](
	      m.getName,
	      List( fromCC( m.invoke( cc ) ) )
	    )
	  }).toList

	new CnxnCtxtBranch[String,String,String] (
	  cc.getClass.getName,
	  facts
	)
      }
    }
    fromCC( cc )
  }

  def fromXML( 
    lbl2Namespace : String => Namespace,
    text2Var : String => Var,
    text2Tag : String => Tag
  )(
    cciElem : Node
  ) : Option[CnxnCtxtLabel[Namespace,Var,Tag] with Factual] = {
    cciElem match {
      case e : Elem => {
	Some(
	  new CnxnCtxtBranch[Namespace,Var,Tag](
	    lbl2Namespace( cciElem.label ),
	    for(
	      child <- cciElem.child.toList;
	      childVal <- 
	      fromXML(
		lbl2Namespace,
		text2Var,
		text2Tag
	      )( child )
	    ) 
	    yield {
	      childVal
	    }
	  )
	)
      }
      case Text( contents ) => {
	//println( "text node with contents = " + contents )
	if ( java.util.regex.Pattern.matches( "(\\p{Space}|\\p{Blank})*", contents ) ) {
	  //println( "contents is whitespace " )
	  None
	}
	else {
	  if ( contents.substring( 0, 1 ) == "'" ) {
	    Some(
	      new CnxnCtxtLeaf[Namespace,Var,Tag](
		Right( text2Var( contents ) )
	      )
	    )
	  }
	  else {
	    Some(
	      new CnxnCtxtLeaf[Namespace,Var,Tag](
		Left( text2Tag( contents ) )
	      )
	    )
	  }
	}
      }
      case _ => {
	None
      }
    }
  }
  
  def fromXML( 
    lbl2Namespace : String => Namespace,
    text2Var : String => Var
  )(
    cciElem : Node
  ) : Option[CnxnCtxtLabel[Namespace,Var,String] with Factual] = {
    cciElem match {
      case e : Elem => {
	Some(
	  new CnxnCtxtBranch[Namespace,Var,String](
	    lbl2Namespace( cciElem.label ),
	    for(
	      child <- cciElem.child.toList;
	      childVal <- 
	      fromXML(
		lbl2Namespace,
		text2Var
	      )( child )
	    ) 
	    yield {
	      childVal
	    }
	  )
	)
      }
      case Text( contents ) => {
	//println( "text node with contents = " + contents )
	if ( java.util.regex.Pattern.matches( "(\\p{Space}|\\p{Blank})*", contents ) ) {
	  //println( "contents is whitespace " )
	  None
	}
	else {
	  if ( contents.substring( 0, 1 ) == "'" ) {
	    Some(
	      new CnxnCtxtLeaf[Namespace,Var,String](
		Right( text2Var( contents ) )
	      )
	    )
	  }
	  else {
	    Some(
	      new CnxnCtxtLeaf[Namespace,Var,String](
		Left( contents )
	      )
	    )
	  }
	}
      }
      case _ => {
	None
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
    def root : Option[CnxnCtxtBranch[Namespace,Var,Tag]]
    def branch : CnxnCtxtLabel[Namespace,Var,Tag]
    def xqVar : String
    def letVar : Option[String]
    def parent : Option[XQueryCompilerContext]
    def location : Option[Location[Either[Tag,Var]]]
    def index : DeBruijnIndex
    
    def sibling(
      branch : CnxnCtxtLabel[Namespace,Var,Tag],
      nxqv : String,
      lxqv : Option[String]
    ) : XQueryCompilerContext

    def child(
      branch : CnxnCtxtLabel[Namespace,Var,Tag],
      nxqv : String,
      lxqv : Option[String]
    ) : XQueryCompilerContext

    def child(
      branch : CnxnCtxtLabel[Namespace,Var,Tag],
      nxqv : String,
      lxqv : Option[String],
      width : Int
    ) : XQueryCompilerContext
  }

  case class XQCC( 
    override val root : Option[CnxnCtxtBranch[Namespace,Var,Tag]],
    override val branch : CnxnCtxtLabel[Namespace,Var,Tag],
    override val xqVar : String,
    override val letVar : Option[String],
    override val parent : Option[XQueryCompilerContext],
    override val location : Option[Location[Either[Tag,Var]]],
    override val index : DeBruijnIndex
  ) extends XQueryCompilerContext {
    override def sibling(
      branch : CnxnCtxtLabel[Namespace,Var,Tag],
      nxqv : String,
      lxqv : Option[String]
    ) =
      XQCC(
	root, branch, nxqv, lxqv, parent, location,
	DeBruijnIndex( index.depth, index.width + 1 )
      )
    override def child(
      branch : CnxnCtxtLabel[Namespace,Var,Tag],
      nxqv : String,
      lxqv : Option[String]
    ) =
      XQCC(
	root, branch, nxqv, lxqv, Some( this ), location, 
	DeBruijnIndex( index.depth + 1, 0 )
      )
    override def child(
      branch : CnxnCtxtLabel[Namespace,Var,Tag],
      nxqv : String,
      lxqv : Option[String],
      width : Int
    ) =
      XQCC(
	root, branch, nxqv, lxqv, Some( this ), location, 
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

  def xqRecExistentialConstraints(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
  ) : String = {

    val width = xqcc.index.width
    val depth = xqcc.index.depth
    val xqVar = xqcc.xqVar

    println( "entering xqRecExistentialConstraints with " )
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
	      val fxqcc = xqcc.child( fact, nxqv, None, 0 )
	      val factC = xqRecExistentialConstraints( fact, fxqcc )
	      ( ( factC, 1 ) /: fs )( 
		{
		  ( acc, f ) => {
		    val ( ccs, w ) = acc
		    val nxqcc = xqcc.child( f, nxqv, None, w )
		    val fC = xqRecExistentialConstraints( f, nxqcc )
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

  def xqLetBinding(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext,
    nxqv : String,
    w : Int
  ) : Option[( Option[String], String )] = {
    ccl match {
      case CnxnCtxtLeaf( Right( _ ) ) => None
      case _ => {
	val lxqv = nextXQV
	val fLE = ( lxqv + " := " + nxqv + "/" + "*" + "[" + (w+1) + "]" )
	
	Some( ( Some( lxqv ), fLE ) )
      }      
    }    
  }

  def xqWrapTerm(
    ccl : CnxnCtxtLeaf[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
  ) : String = {
    ccl match {
      case ccf : CnxnCtxtLeaf[Namespace,Var,Tag] =>
	asXMLData( ccf ).toString
    }
  }

  def xqRecConstraints(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
  ) : String = {

    val width = xqcc.index.width
    val depth = xqcc.index.depth
    val xqVar = xqcc.xqVar
    val letVar = xqcc.letVar

    ccl match {
      case ccf@CnxnCtxtLeaf( Left( t ) ) =>
	( letVar.getOrElse( nextXQV ) + " = " + xqWrapTerm( ccf, xqcc ) )
      case CnxnCtxtLeaf( Right( v ) ) =>
	( "" )
      case CnxnCtxtBranch( ns, facts ) => {
	val nxqv = nextXQV

	val ( ( childLEs, childCs ), _ ) =
	  facts match {
	    case fact :: fs => {
	      val ( lxqv, factLE ) = 
		xqLetBinding( fact, xqcc, nxqv, 0 )
	        .getOrElse( ( None, "" ) )

	      val fxqcc = xqcc.child( fact, nxqv, lxqv, 0 )
	      val factC =
		xqRecConstraints( fact, fxqcc ) match {
		  case "" => ""
		  case fC@_ =>
		    "(" + " " + fC + " " + ")"
		}	

	      ( ( ( factLE, factC ), 1 ) /: fs )( 
		{
		  ( acc, f ) => {
		    val ( ( les, ccs ), w ) = acc
		    val ( nlxqv, fLE ) = 
		      xqLetBinding( fact, xqcc, nxqv, w )
	              .getOrElse( ( None, "" ) )
		    val nxqcc = xqcc.child( f, nxqv, nlxqv, w )
		    val fC = xqRecConstraints( f, nxqcc )
		    val accLEs =		      
		      les match {
			case "" => fLE
			case _ => les + " , " + fLE
		      }
		    val accfC =
		      fC match {
			case "" => ccs 
			case _ =>
			  ccs + " and " + "(" + " " + fC + " " + ")"
		      }

		    ( ( accLEs, accfC ), w+1 )
		  }
		}
	      )
	    }
	    case Nil => ( "", 0 )
	  }	

	val arityC = (
	  "(" + " " + "count" + "(" + nxqv + "/" + "*" + ")" 
	  + " = " + facts.length + " " + ")"
	)

	val letEs =
	  childLEs match {
	    case "" => ""
	    case _ => " let " + childLEs
	  }

	val existentialC = (
	  "for" + " " + nxqv + " in " + xqVar + "/" + ns
	  + letEs
	  + " where "
	  + (
	    childCs match {
	      case "" =>
		(
		  arityC
		  + (
		    ( letVar, xqcc.parent ) match {
		      case ( Some( v ), Some( _ ) ) =>
			" and " + "(" + " " + v + " = " + nxqv + " " + ")"
		      case _ => ""
		    }
		  )
		)
	      case _ =>
		(
		  arityC + " "
		  + " and "
		  + childCs
		  + (
		    ( letVar, xqcc.parent ) match {
		      case ( Some( v ), Some( _ ) ) =>
			" and " + "(" + " " + v + " = " + nxqv + " " + ")"
		      case _ => ""
		    }
		  )
		)
	    } 
	  )
	  + " return " + nxqv
	)		  
	
        existentialC
      }
    }
  }  

  def xqQuery(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    xqcc : XQueryCompilerContext
  ) : String = {
    xqRecConstraints( ccl, xqcc )
  }
  
  def xqQuery(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : String = {
    ccl match {
      case CnxnCtxtLeaf( _ ) => {	  
	val nxqv = nextXQV
	val xqcc =
	  XQCC(
	    None,
	    ccl,
	    "/", Some( nxqv ),
	    None, None,
	    DeBruijnIndex( 0, 0 )
	  )
	val cond =
	  xqRecConstraints( ccl, xqcc )
	(
	  "for" + " " + nxqv + " in " + "/root" + " where " + cond 
	  + " return "  + nxqv
	)
      }
      case ccb : CnxnCtxtBranch[Namespace,Var,Tag] => {			
	val xqcc =
	  XQCC(
	    Some( ccb ),
	    ccl,
	    "/", Some( nextXQV ),
	    None, None,
	    DeBruijnIndex( 0, 0 )
	  )
	xqRecConstraints( ccl, xqcc )
      }
    }    
  }
}




