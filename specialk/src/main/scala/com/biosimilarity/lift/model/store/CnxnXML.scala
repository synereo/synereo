// -*- mode: Scala;-*- 
// Filename:    CnxnXML.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 10 03:57:46 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.zipper._

import scala.xml._
import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

trait Blobify {
  def toBlob( x : java.lang.Object ) : String = {
    toJSONBlob( x )
  }
  def fromBlob( blob : String ) : java.lang.Object = {
    fromJSONBlob( blob )
  }      
  def toJSONBlob( x : java.lang.Object ) : String = {
    new XStream( new JettisonMappedXmlDriver() ).toXML( x )
  }
  def fromJSONBlob( blob : String ) : java.lang.Object = {
    new XStream( new JettisonMappedXmlDriver() ).fromXML( blob )
  }      
  def toXQSafeJSONBlob( x : java.lang.Object ) : String = {
    val jsonBlob =
      new XStream( new JettisonMappedXmlDriver() ).toXML( x )
    jsonBlob.replace(
      "{",
      "{{"
    ).replace(
      "}",
      "}}"
    )
  }
  def fromXQSafeJSONBlob( blob : String ) : java.lang.Object = {
    val jsonBlob =
      (if ( blob.substring( 0, 2 ).equals( "{{" ) ) {
	blob.replace(
	  "{{",
	  "{"
	).replace(
	  "}}",
	  "}"
	)
      }
      else {
	blob
      }).replace(
      "&quot;",
      "\""
    )
    new XStream( new JettisonMappedXmlDriver() ).fromXML( jsonBlob )
  }      
  def toXMLBlob( x : java.lang.Object ) : String = {
    new XStream( ).toXML( x )
  }
  def fromXMLBlob( blob : String ) : java.lang.Object = {
    new XStream( ).fromXML( blob )
  }      
}

trait CnxnConcreteToAbstractSyntax[Namespace,Var,Tag] {
  def fromCaseClassInstanceString(
    cciElem : String
  ) : Option[CnxnCtxtLabel[String,String,Any]]
  def fromTermString(
    cciElem : String
  ) : Option[CnxnCtxtLabel[String,String,String]]
}

trait CnxnStringX[Namespace,Var,Tag]
extends CnxnConcreteToAbstractSyntax[Namespace,Var,Tag] {
  class TermParser extends JavaTokenParsers {
    def term : Parser[Any] =
      application | list | ground | variable
    def list : Parser[Any] =
      "["~repsep( term, "," )~"]"
    def ground : Parser[Any] =
      stringLiteral | floatingPointNumber | "true" | "false"
    def variable : Parser[Any] = ident
    def application : Parser[Any] =
      ident~"("~repsep( term, "," )~")"

    def termXform : Parser[CnxnCtxtLabel[String,String,Any] with Factual] =
      applicationXform | listXform | groundXform | variableXform
    def listXform : Parser[CnxnCtxtLabel[String,String,Any] with Factual] = 
      "["~repsep( termXform, "," )~"]" ^^ {
	case "["~terms~"]" => {
	  new CnxnCtxtBranch[String,String,Any](
	    "list",
	    terms
	  )
	}
      }
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

    def termXStr : Parser[CnxnCtxtLabel[String,String,String] with Factual] =
      applicationXStr | listXStr | groundXStr | variableXStr
    def listXStr : Parser[CnxnCtxtLabel[String,String,String] with Factual] = 
      "["~repsep( termXStr, "," )~"]" ^^ {
	case "["~terms~"]" => {
	  new CnxnCtxtBranch[String,String,String](
	    "list",
	    terms
	  )
	}
      }
    def groundXStr : Parser[CnxnCtxtLabel[String,String,String] with Factual] =
      (
	stringLiteral ^^ ( x => new CnxnCtxtLeaf[String,String,String]( Left[String,String]( x.replace( "\"", "" ) ) ) )
	| wholeNumber ^^ ( x => new CnxnCtxtLeaf[String,String,String]( Left[String,String]( x + "" ) ) )
	| floatingPointNumber ^^ ( x => new CnxnCtxtLeaf[String,String,String]( Left[String,String]( x.toDouble + "" ) ) )
	| "true" ^^ ( x => new CnxnCtxtLeaf[String,String,String]( Left[String,String]( "true" ) ) )
	| "false" ^^ ( x => new CnxnCtxtLeaf[String,String,String]( Left[String,String]( "false" ) ) )
      )
    def variableXStr : Parser[CnxnCtxtLabel[String,String,String] with Factual] =
      ident ^^ ( x => new CnxnCtxtLeaf[String,String,String]( Right( x.toString ) ) )
    def applicationXStr : Parser[CnxnCtxtLabel[String,String,String] with Factual] =
      ident~"("~repsep( termXStr, "," )~")" ^^ {
	case ident~"("~terms~")" => new CnxnCtxtBranch[String,String,String]( ident, terms )
      }
  }

  override def fromCaseClassInstanceString(
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
  override def fromTermString(
    cciElem : String
  ) : Option[CnxnCtxtLabel[String,String,String]] = {
    val readBack = new TermParser
    val ptree =
      readBack.parseAll(
	readBack.termXStr,
	new java.io.StringReader( cciElem )
      )
    ptree match {
      case readBack.Success( r, _ ) => Some( r )
      case _ => None
    }
  }
}

trait CnxnCtxtHash[Namespace,Var,Tag] 
extends CnxnConcreteToAbstractSyntax[Namespace,Var,Tag] {
  class TermParser extends JavaTokenParsers {
    def rpnTerm : Parser[Any] =
      path | combination
    def path : Parser[Any] =
      "["~repsep( stringLiteral, "," )~"]"
    def combination : Parser[Any] =
      anyCombo | allCombo | eachCombo
    def anyCombo : Parser[Any] =
      "any"~"("~repsep( rpnTerm, "," )~")"
    def allCombo : Parser[Any] =
      "all"~"("~repsep( rpnTerm, "," )~")"
    def eachCombo : Parser[Any] =
      "each"~"("~repsep( rpnTerm, "," )~")"

    def rpnTermXform : Parser[CnxnCtxtLabel[String,String,Any] with Factual] =
      pathX | combinationX

    def pathLoop(
      steps : List[String]
    ) : CnxnCtxtLabel[String,String,Any] with Factual = {
      steps match {
	case step :: rSteps => {
	  val terminal : CnxnCtxtLabel[String,String,Any] with Factual =
	    new CnxnCtxtLeaf[String,String,Any]( Right( "_" ) )
	  ( steps :\ terminal )( 
	    {
	      ( e : String, acc : CnxnCtxtLabel[String,String,Any] with Factual ) => {
		new CnxnCtxtBranch[String,String,Any](
		  e,
		  List[CnxnCtxtLabel[String,String,Any] with Factual]( acc )
		)
	      }
	    }
	  )
	}
	case Nil => {
	  throw new Exception( "empty path" )
	}
      }
    }
    def pathX : Parser[CnxnCtxtLabel[String,String,Any] with Factual] = 
      "["~repsep( stringLiteral, "," )~"]" ^^ {
	case "["~steps~"]" => {
	  pathLoop( steps )
	}
      }
    def combinationX : Parser[CnxnCtxtLabel[String,String,Any] with Factual] =
      anyComboX | allComboX | eachComboX

    def anyComboX : Parser[CnxnCtxtLabel[String,String,Any] with Factual] =
      "any"~"("~repsep( rpnTermXform, "," )~")" ^^ {
	case "any"~"("~rpnTerms~")" => {
	  new CnxnCtxtBranch[String,String,Any](
	    "any",
	    rpnTerms
	  )
	}
      }
    
    def allComboX : Parser[CnxnCtxtLabel[String,String,Any] with Factual] =
      "all"~"("~repsep( rpnTermXform, "," )~")" ^^ {
	case "all"~"("~rpnTerms~")" => {
	  new CnxnCtxtBranch[String,String,Any](
	    "all",
	    rpnTerms
	  )
	}
      }

    def eachComboX : Parser[CnxnCtxtLabel[String,String,Any] with Factual] =
      "each"~"("~repsep( rpnTermXform, "," )~")" ^^ {
	case "each"~"("~rpnTerms~")" => {
	  new CnxnCtxtBranch[String,String,Any](
	    "each",
	    rpnTerms
	  )
	}
      }    
  }
}

trait CnxnString[Namespace,Var,Tag]
extends CnxnConcreteToAbstractSyntax[Namespace,Var,Tag] {
  import com.biosimilarity.lift.lib.term.conversion._
  import com.biosimilarity.lift.lib.term.Prolog._
  import com.biosimilarity.lift.lib.term.Prolog.Absyn.{Predicate => PrologPredicate,_}

  case class Identity[T]( ) extends Function1[T,T] {
    override def apply( t : T ) = t
  }
  object idS extends Identity[String]
  object CnxnStrZipr
  extends CnxnNavigation[String,String,String]
  with CnxnMutation[String,String,String]
  with CnxnZipperComposition[String,String,String]
  with Serializable

  object ContextVar extends Serializable {
    import java.util.UUID
    val thisContextVar : String =
      "X" + UUID.randomUUID.toString.replace( "-", "" ) + "X"
  }

  case class TermToCCLStr( ) extends TermToCnxnCtxtLabel(
    idS, idS, idS, idS, idS, idS, CnxnStrZipr, ContextVar.thisContextVar
  ) {
    def strToTerm( s : String ) : CnxnCtxtLabel[String,String,String] = {
      val ast = 
	new parser( new Yylex( new java.io.StringReader( s ) ) ).pPredicate()
      val loc : Location[Either[String,String]] =
	Location[Either[String,String]]( 
	  new CnxnCtxtLeaf[String,String,String]( Right[String,String]( text2v( "_" ) ) ),
	  Top()
	)
      val ctxt : Option[Location[Either[String,String]]] = Some( loc )

      val xformedTerm =
	ast match {
	  case apred : APred => visit( apred, ctxt )
	  case cpred : CPred => visit( cpred, ctxt )
	}

      xformedTerm match {
	case Some( loc ) => zipr.decontextualize( loc )
	case None => throw new Exception( "xform failed: " + ast )
      }
    }
  }

  override def fromCaseClassInstanceString(
    cciElem : String
  ) : Option[CnxnCtxtLabel[String,String,Any]] = {
    Some(
      (
	new TermToCCLStr().strToTerm(
	  cciElem
	)
      ).asInstanceOf[CnxnCtxtLabel[String,String,Any]]
    )
  }
  override def fromTermString(
    cciElem : String
  ) : Option[CnxnCtxtLabel[String,String,String]] = {
    Some( new TermToCCLStr().strToTerm( cciElem ) )
  }
}

trait CnxnXML[Namespace,Var,Tag] extends CnxnString[Namespace,Var,Tag] {
  self : CnxnCtxtInjector[Namespace,Var,Tag]	 
	 with Blobify with UUIDOps =>

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

	val lcTagStr =
	  (
	    tagStr.substring( 0, 1 ).toLowerCase
	    + tagStr.substring( 1, tagStr.length )
	  )

	val tStr = 
	  t match {
	    case s : String => s
	    case _ => t.toString
	  }

	xmlTrampoline( lcTagStr, tStr )
      }
      case Right( v ) => {
	<var>{ v.toString }</var>
      }
    }
  }

  def asXMLData(
    branch : CnxnCtxtBranch[Namespace,Var,Tag]
  ) : Node = {
    val tagStr = branch.nameSpace + ""
    val ctagStr = tagStr.replace( "'", "" )    
    
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

    xmlTrampoline( ctagStr, fx )    
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
	t match {
	  case s : String => {
	    "\"" + s + "\""
	  }
	  case _ => t.toString
	}	
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

  def toJSON(
    cnxn : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : String = {
    cnxn match {
      case leaf : CnxnCtxtLeaf[Namespace,Var,Tag] =>
	toJSONStr( leaf )
      case branch : CnxnCtxtBranch[Namespace,Var,Tag] =>
	"{ " + toJSONStr( branch ) + " }"
    }
  }

  def toJSONStr(
    leaf : CnxnCtxtLeaf[Namespace,Var,Tag]
  ) : String = {
    leaf.tag match {
      case Left( t ) => {
	t match {
	  case s : String => {
	    "\"" + s + "\""
	  }
	  case _ => t.toString
	}	
      }
      case Right( v ) => {
	v.toString
      }
    }
  }

  def toJSONStr(
    branch : CnxnCtxtBranch[Namespace,Var,Tag]
  ) : String = {
    val tagStr = branch.nameSpace
    val fx = 
      branch.factuals match {
	case fact :: rfacts => {
	  ( toJSON( fact ) /: rfacts )(
	    {
	      ( acc, f ) => {
		acc + "," + toJSON( f )
	      }
	    }
	  )
	}
	case Nil => {
	  ""
	}
      }

    tagStr + " : " + "[ " + fx + " ]"
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
     || (value.isInstanceOf[Long]) 
     || (value.isInstanceOf[Float])
     || (value.isInstanceOf[Double])
     || (value.isInstanceOf[String])
     || ((value.isInstanceOf[List[_]]) && (value.asInstanceOf[List[_]].isEmpty))
     //|| (value.isInstanceOf[Option[_]])
     // put more ground types here
   )
  }

  def blobLabel : String = "cclBlob"

  def tolabeledBlob [Namespace,Var,Tag]  (
    labelToNS : String => Namespace,
    valToTag : String => Tag
  )(
    cc : Product
  ) : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
    new CnxnCtxtBranch [Namespace,Var,Tag] (
      labelToNS( blobLabel ),
      List( 
	new CnxnCtxtLeaf [Namespace,Var,Tag] (
	  Left( valToTag( toBlob( cc.asInstanceOf[AnyRef] ) ) )
	)
      )
    )
  }

  def fromlabeledBlob [Namespace,Var,Tag] (
    cclBlob : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : Option[Product] = {
    cclBlob match {
      case CnxnCtxtBranch( blNS, CnxnCtxtLeaf( Left( blob ) ) :: Nil ) => {
	if ( blobLabel.equals( blNS.toString ) ) {
	  Some( fromBlob( blob + "" ).asInstanceOf[Product] )
	}
	else {
	  None
	}
      }
      case _ => {
	None
      }
    }
  }

  def caseClassAccessors( 
    //cc : ScalaObject with Product with Serializable
    cc : java.lang.Object
  ) : /* Array[java.lang.reflect.Method] */ Array[java.lang.reflect.Field] = {
    // this is what you call a heuristic
    // and heuristic is probably better than myistic 	      
//     cc.getClass.getMethods.filter(
//       ( m : java.lang.reflect.Method ) => {      
// 	((! javaBuiltins.contains( m.getName ) )
// 	 && (( m.getParameterTypes.size ) == 0)
// 	 && (!java.util.regex.Pattern.matches( "product.*" , m.getName ))
// 	 && (!java.util.regex.Pattern.matches( "copy.default.*" , m.getName ))
// 	 && (! java.lang.reflect.Modifier.isStatic( m.getModifiers() ) )
//        )
//       }
//     )
    cc.getClass.getDeclaredFields
  }

  def nameStrCleansing( initialName : String ) : String = {
    val nameComponents = initialName.split( "\\." ).toList
    val ccns =
      if ( nameComponents.length > 1 ) {
	val iC =
	  nameComponents.take( 1 )( 0 )
	val initialCompStr =
	  iC.substring( 0, 1 ).toLowerCase + iC.substring( 1, iC.length )
	
	( initialCompStr /: nameComponents.drop( 1 ) )(
	  {
	    ( acc, e ) => {
	      val compStr =
		e.substring( 0, 1 ).toUpperCase + e.substring( 1, e.length ) 
	      acc + compStr
	    }
	  }
	)
      }
      else {
	initialName
      }
    ccns.replace( "$", "_" )
  }

  def caseClassNameSpace(
    //cc : ScalaObject with Product with Serializable
    cc : java.lang.Object
  ) : String = {
    nameStrCleansing( cc.getClass.getName )
  }

  def fromCaseClass [Namespace,Var,Tag] (
    filter : java.lang.reflect.Method => Boolean
  )(
    labelToNS : String => Namespace,
    valToTag : java.lang.Object => Tag
  )(
    cc : ScalaObject with Product with Serializable
  ) : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {    
    def fromCC(
      cc : java.lang.Object
    ) : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
      if ( isGroundValueType( cc ) ) {
	new CnxnCtxtLeaf[Namespace,Var,Tag](
	  Left( valToTag( cc ) )
	)
      }
      else {
	if ( cc.isInstanceOf[Option[_]] ) {
	  cc match {
	    case Some(
	      thing : ScalaObject with Product with Serializable
	    ) => {
	      new CnxnCtxtBranch[Namespace,Var,Tag](
		labelToNS( "some" ),
		List(
		  fromCaseClass( filter )( labelToNS, valToTag )( thing )
		)
	      )
	    }
	    case Some(
	      thingElse : AnyRef
	    ) => {
	      new CnxnCtxtLeaf[Namespace,Var,Tag](
		Left( valToTag( thingElse ) )
	      )
	    }
	    case None => {
	      new CnxnCtxtLeaf[Namespace,Var,Tag](
		Left( valToTag( "none" ) )
	      )
	    }
	  }	  
	}
	else {
	  val facts =
	    (
	      for( m <- caseClassAccessors( cc ) ) yield {
		val faccess = m.isAccessible
		m.setAccessible( true )
		val fval = m.get( cc )
		m.setAccessible( faccess )

		new CnxnCtxtBranch[Namespace,Var,Tag](
		  labelToNS( nameStrCleansing( m.getName ) ),
		  //List( fromCC( m.invoke( cc ) ) )
		  List( fromCC( fval ) )
		)
	      }
	    ).toList
	  
	  new CnxnCtxtBranch[Namespace,Var,Tag] (
	    labelToNS( caseClassNameSpace( cc ) ),
	    facts
	  )
	}
      }
    }
    fromCC( cc )
  }

  def partialCaseClassDerivative [Namespace,Var,Tag] (
    filter : java.lang.reflect.Method => Boolean
  )(
    labelToNS : String => Namespace,
    valToTag : java.lang.Object => Tag,
    strToVar : String => Var
  )(
    cc : ScalaObject with Product with Serializable,
    vars : List[(String,String)]
  ) : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {    
    val varMap = new HashMap[String,String]()
    for( ( fld, v ) <- vars ) { varMap += ( fld -> v ) }
    val varFlds = varMap.keys

    def fromCC(
      cc : java.lang.Object
    ) : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
      //println( "in fromCC with " + cc )
      BasicLogService.tweet( "in fromCC with " + cc )
      if ( isGroundValueType( cc.asInstanceOf[AnyRef] ) ) {	
	//println( cc + " is a ground value" )
        BasicLogService.tweet( cc + " is a ground value" )
	new CnxnCtxtLeaf[Namespace,Var,Tag](
	  Left( valToTag( cc ) )
	)
      }
      else {
	if ( cc.isInstanceOf[Option[_]] ) {
	  //println( cc + " is an option" )
          BasicLogService.tweet( cc + " is an option" )
	  cc match {
	    case Some(
	      thing : ScalaObject with Product with Serializable
	    ) => {
	      //println( cc + " is Some( <case class instance> )" )
              BasicLogService.tweet( cc + " is Some( <case class instance> )" )
	      new CnxnCtxtBranch[Namespace,Var,Tag](
		labelToNS( "some" ),
		List(
		  partialCaseClassDerivative( filter )( labelToNS, valToTag, strToVar )( thing, vars )
		)
	      )
	    }
	    case Some(
	      thingElse : AnyRef
	    ) => {
	      //println( cc + " is Some( <some other kind of instance>
              //)" )
              BasicLogService.tweet( cc + " is Some( <some other kind of instance> )" )
	      new CnxnCtxtLeaf[Namespace,Var,Tag](
		Left( valToTag( thingElse ) )
	      )
	    }
	    case None => {
	      //println( cc + " is None" )
              BasicLogService.tweet( cc + " is None" )
	      new CnxnCtxtLeaf[Namespace,Var,Tag](
		Left( valToTag( "none" ) )
	      )
	    }
	  }	  
	}
	else {	  	  
	  //println( cc + " is a plain old case class" )
          BasicLogService.tweet( cc + " is a plain old case class" )
	  //println( "iterating through fields" )
          BasicLogService.tweet( "iterating through fields" )
	  val facts =
	    (
	      for(
		m <- caseClassAccessors( cc )
	      ) yield {
		varMap.get( m.getName ) match {
		  case Some( v ) => {
		    //println( cc + "'s field, " + m.getName + " is in
                    //the list of vars" )
                    BasicLogService.tweet( cc + "'s field, " + m.getName + " is in the list of vars" )
		    new CnxnCtxtLeaf[Namespace,Var,Tag](
		      Right( strToVar( v ) )
		    )
		  }
		  case None => {
		    //println( cc + "'s field, " + m.getName + " is
                    //not in the list of vars" )
                    BasicLogService.tweet( cc + "'s field, " + m.getName + " is not in the list of vars" )
		    val faccess = m.isAccessible
		    m.setAccessible( true )
		    val fval = m.get( cc )
		    m.setAccessible( faccess )
		    
		    //println( "recursing on the value, " + fval + ",
                    //of " + cc + "'s field, " + m.getName + " is not
                    //in the list of vars" )
                    BasicLogService.tweet( "recursing on the value, " + fval + ", of " + cc + "'s field, " + m.getName + " is not in the list of vars" )
		    new CnxnCtxtBranch[Namespace,Var,Tag](
		      labelToNS( nameStrCleansing( m.getName ) ),
		      //List( fromCC( m.invoke( cc ) ) )
		      fval match {
			case fvs : List[_] => fvs.map( ( x ) => fromCC( x.asInstanceOf[AnyRef] ) )
			case _ => List( fromCC( fval ) )
		      }
		    )
		  }
		}		
	      }
	    ).toList
	  
	  new CnxnCtxtBranch[Namespace,Var,Tag] (
	    labelToNS( caseClassNameSpace( cc ) ),
	    facts
	  )
	}
      }
    }
    fromCC( cc )
  }

  def fromCaseClass [Namespace,Var,Tag] (
    labelToNS : String => Namespace,
    valToTag : java.lang.Object => Tag
  )(
    cc : ScalaObject with Product with Serializable
  ) : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
    fromCaseClass( ( m : java.lang.reflect.Method ) => true )( labelToNS, valToTag )( cc )
  }

  def fromCaseClass(
    cc : ScalaObject with Product with Serializable
  ) : CnxnCtxtLabel[String,String,String] with Factual = {    
    fromCaseClass [String,String,String] (
      ( x : String ) => x,
      ( x : java.lang.Object ) => x.toString      
    )(
      cc 
    )
  }

  def partialCaseClassDerivative(
    cc : ScalaObject with Product with Serializable,
    vars : List[(String,String)]
  ) : CnxnCtxtLabel[String,String,String] with Factual = {    
    partialCaseClassDerivative [String,String,String] (
      ( m :java.lang.reflect.Method ) => true 
    )(
      ( x : String ) => x,
      ( x : java.lang.Object ) => x + "",
      ( x : String ) => x      
    )(
      cc, vars
    )
  }

  def fromXML( 
    lbl2Namespace : String => Namespace,
    text2Var : String => Var,
    text2Tag : String => Tag
  )(
    cciElem : Node
  ) : Option[CnxnCtxtLabel[Namespace,Var,Tag] with Factual] = {
    cciElem match {
      case <var>{ Text( c ) }</var> => {
	Some(
	  new CnxnCtxtLeaf[Namespace,Var,Tag](
	    Right( text2Var( c ) )
	  )
	)
      }
      case e : Elem => {
        //BasicLogService.tweet( "elem with children = " + cciElem.child.toList )
	val attrs =
	  for( m <- cciElem.attributes ) 
	    yield {
	      new CnxnCtxtBranch[Namespace,Var,Tag](
		lbl2Namespace( m.key + "@" ),
		for( 
		  attrVN <- m.value.toList;
		  attrCCL <- 
		  fromXML(
		    lbl2Namespace,
		    text2Var,
		    text2Tag
		  )( attrVN )
		)
		yield {
		  attrCCL
		}
	      )
	    }
	val progeny =
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
	Some(
	  new CnxnCtxtBranch[Namespace,Var,Tag](
	    lbl2Namespace( cciElem.label ),
	    attrs.toList ++ progeny.toList
	  )
	)
      }
      case Text( contents ) => {
	//println( "text node with contents = " + contents )
	if (
	  java.util.regex.Pattern.matches( 
	    "(\\p{Space}|\\p{Blank})*",
	    contents
	  )
	) {
	  //println( "contents is whitespace " )
	  None
	}
	else {
	  if ( contents.substring( 0, 1 ) == "'" ) {
	    //println( "contents make a variable name " )
	    Some(
	      new CnxnCtxtLeaf[Namespace,Var,Tag](
		Right( text2Var( contents ) )
	      )
	    )
	  }
	  else {
	    //println( "contents comprise a string literal " )
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

trait CnxnScalesXML[Namespace,Var,Tag] {
  self : CnxnXML[Namespace,Var,Tag] =>

    import scales.xml.{
      Doc => SXMLDoc, Namespace => SXMLNamespace, Elem => SXMLElem, Text => SXMLText, _
    }
    import scales.utils.{ Tree => SXMLTree, _ }
    import ScalesUtils._
    import ScalesXml._
    import Functions._

  def fromScalesXML( 
    lbl2Namespace : String => Namespace,
    text2Var : String => Var,
    text2Tag : String => Tag
  )(
    doc : SXMLDoc
  ) : Option[CnxnCtxtLabel[Namespace,Var,String] with Factual] = {
    val tree : SXMLTree[XmlItem,SXMLElem,XCC] = doc.rootElem
    fromScalesXMLTree( lbl2Namespace, text2Var, text2Tag )( tree )
  }
  
  def fromScalesXMLTree( 
    lbl2Namespace : String => Namespace,
    text2Var : String => Var,
    text2Tag : String => Tag
  )(
    tree : SXMLTree[XmlItem,SXMLElem,XCC]
  ) : Option[CnxnCtxtLabel[Namespace,Var,String] with Factual] = {
    tree.section.name.qName match {
      case "var" => {
	Some(
	  new CnxnCtxtLeaf[Namespace,Var,String](
	    Right( text2Var( text( tree ) ) )
	  )
	)
      }
      case qName => {
	val projeny =
	  for {
	    c <- tree.children.toList
	    childVal <- 
	    c.isRight match {
	      case true => {
		fromScalesXMLTree( lbl2Namespace, text2Var, text2Tag )( c.getRight )
	      }
	      case false => {
		fromScalesXMLLeaf( lbl2Namespace, text2Var, text2Tag )( c.getLeft )
	      }
	    }
	  }
	  yield childVal
	
	Some( 
	  new CnxnCtxtBranch[Namespace,Var,String](
	    lbl2Namespace( qName ),
	    projeny.toList
	  )
	)
      }
    }
  }  

  def fromScalesXMLLeaf( 
    lbl2Namespace : String => Namespace,
    text2Var : String => Var,
    text2Tag : String => Tag
  )(
    elem : XmlItem
  ) : Option[CnxnCtxtLabel[Namespace,Var,String] with Factual] = {
    Some(
      new CnxnCtxtLeaf[Namespace,Var,String](
	Left( elem.value )
      )
    )
  }

}

trait CnxnConversionScope[Namespace,Var,Tag] {
  type CnxnConversionType <: CnxnXML[Namespace,Var,Tag]
  def protoCnxnConversions : CnxnConversionType
  val cnxnConversions = protoCnxnConversions
}

object CnxnConversionStringScope
   extends CnxnConversionScope[String,String,String]
{
  override type CnxnConversionType =
    CnxnXML[String,String,String]
      with CnxnCtxtInjector[String,String,String]
      with Blobify 
      with UUIDOps
  object theCnxnConversions
     extends CnxnXML[String,String,String]
      with CnxnCtxtInjector[String,String,String]
      with Blobify 
      with UUIDOps
  override def protoCnxnConversions = theCnxnConversions

  def l2ns( s : String ) = {
    // Heuristic to defeat REPL
    val sp =
      if (java.util.regex.Pattern.matches( ".*line.*read.*", s )) {
	s.substring( s.lastIndexOf( "$" ) + 1, s.length )
      }
      else {
	s
      }
    ( sp.substring( 0, 1 ).toLowerCase + sp.substring( 1, sp.length ) )
  }
  def v2t( obj: java.lang.Object ) : String = {
    obj match {
      //case "" => "\"\""
      case s : String => {
	s
      }      
      case _ => obj + ""
    }
  }
  def s2v( s : String ) : String = s

  implicit def asCnxnCtxtLabel(
    s : String
  ) : CnxnCtxtLabel[String,String,String]  with Factual = {
    cnxnConversions.fromCaseClassInstanceString(
      s
    ).getOrElse(
      null
    ).asInstanceOf[CnxnCtxtLabel[String,String,String] with Factual]
  }
  
  implicit def asCnxnCtxtLabel(
    cc : ScalaObject with Product with Serializable
  ) : CnxnCtxtLabel[String,String,String] with Factual = {        
    cnxnConversions.fromCaseClass( l2ns, v2t )( cc )
  }

  implicit def partialCaseClassDerivative(
    cc : ScalaObject with Product with Serializable,
    vars : List[(String,String)]
  ) : CnxnCtxtLabel[String,String,String] with Factual = {        
    cnxnConversions.partialCaseClassDerivative( ( m : java.lang.reflect.Method ) => true )( l2ns, v2t, s2v )( cc, vars )
  }

  implicit def asCnxnCtxtLabel(
    e : Elem
  ) : CnxnCtxtLabel[String,String,String]  with Factual = {
    cnxnConversions.fromXML( l2ns, v2t )( e ).getOrElse(
      null
    )
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

    //println( "entering xqRecExistentialConstraints with " )
    //println( "xqVar : " + xqVar )
    //println( "width : " + width )
    //println( "depth : " + depth )

    ccl match {
      case CnxnCtxtLeaf( Left( t ) ) =>
	( xqVar + "/" + "*" + "[" + width + "]" + " = " + t + "" )
      case CnxnCtxtLeaf( Right( v ) ) =>
	( "" )
      case CnxnCtxtBranch( ns, facts ) => {
	val nxqv = nextXQV
	//println( "generated next var: " + nxqv )

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

  def xqCompVarConstraints(
    pfxqv : String, cfxqv : String, olxqv : Option[String]
  ) : Option[String] = {
    for( lxqv <- olxqv ) yield {
      (
	"for" + " " + cfxqv + " " + "in" + " " + pfxqv + "/var"
	+ " " + "where" + " "
	+ "(" + " " + lxqv + " " + "=" + " " + cfxqv + " " + ")"
	+ " " + "return" + " " + cfxqv
      )
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
      case ccf@CnxnCtxtLeaf( Left( t ) ) => {
	val lVT = letVar.getOrElse( nextXQV )
	(
	  "( "
	  + "( "
	  + ( lVT + " = " + xqWrapTerm( ccf, xqcc ) )
	  + " )"
	  + " or "
	  + "( "
	  + "fn:string( "
	  + "fn:node-name( " + lVT + " )"
	  + " )"
	  + " = " + "\"var\""
	  + " )"
	  + " )"
	)
      }
      case CnxnCtxtLeaf( Right( v ) ) =>
	( "1 = 1" ) // the no-op -- true, it turns out, trips a bug in BaseX
      case CnxnCtxtBranch( ns, facts ) => {
	val nxqv = nextXQV
	val nxqcv = nextXQV

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
		  case fC@_ => {
		    xqCompVarConstraints( nxqv, nxqcv, lxqv ) match {
		      case Some( fCV ) => {
			(
			  "(" + " "
			  + "(" + " " + fC + " " + ")"
			  + " " + "or" + " "
			  + "(" + " " + fCV + " " + ")"
			  + " " + ")"
			)
		      }
		      case None => {
			"(" + " " + fC + " " + ")"
		      }
		    }		    
		  }
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
			case _ => {			  
			  val ans = 
			    xqCompVarConstraints( nxqv, nxqcv, lxqv ) match {
			      case Some( fCV ) => {
				(
				  ccs + " and "
				  + "(" + " "
				  + "(" + " " + fC + " " + ")"
				  + " " + "or" + " "
				  + "(" + " " + fCV + " " + ")"
				  + " " + ")"
				)
			      }
			      case None => {
				"(" + " " + fC + " " + ")"
			      }
			    }
			  
			  
			  // println(
// 			    "\n******************************************************************\n"
// 			    + "recursive descent, compound 'and' accumulation\n"
// 			    + "ccs : " + "'" + ccs + "'" + " fC : " + fC + "\n"
// 			    + "******************************************************************\n"
// 			  )
			  ans
			}
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
	  "for" + " " + nxqv + " in " + xqVar + "/"
	  + ns.toString.replace( "'", "" )
	  + letEs
	  + " where "
	  + (
	    childCs match {
	      case "" => {
		val rVC = 
		  ( letVar, xqcc.parent ) match {
		    case ( Some( v ), Some( _ ) ) => {
		      " and " + "(" + " " + v + " = " + nxqv + " " + ")"
		    }
		    case _ => ""
		  }

		val ans = arityC + rVC

		// println(
// 		  "\n******************************************************************\n"
// 		  + "existential constraints case 1, compound 'and' accumulation\n"
// 		  + "arityC : " + arityC + "childCs : " + childCs + "\n"
// 		  + "rVC : " + "'" + rVC + "'" + "\n"
// 		  + "******************************************************************\n"
// 		)

		ans
	      }
	      case _ => {
		val rVC =
		  ( letVar, xqcc.parent ) match {
		    case ( Some( v ), Some( _ ) ) => {
		      " and " + "(" + " " + v + " = " + nxqv + " " + ")"
		    }
		    case _ => ""		    
		  }

		val ans = arityC + " " + " and " + childCs + rVC;

		// println(
// 		  "\n******************************************************************\n"
// 		  + "existential constraints case 2, compound 'and' accumulation\n"
// 		  + "arityC : " + arityC + "childCs : " + childCs + "\n"
// 		  + "rVC : " + "'" + rVC + "'" + "\n"
// 		  + "******************************************************************\n"
// 		)

		ans
	      }
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

  def xqQuery(
    ccl : CnxnCtxtLabel[Namespace,Var,Tag],
    xmlCollStr : String
  ) : String = {
    val ccRootStr =
      "collection( '%COLLNAME%' )/".replace(
	"%COLLNAME%",
	xmlCollStr
      )

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
	  "for" + " " + nxqv + " in " + ccRootStr + " where " + cond 
	  + " return "  + nxqv
	)
      }
      case ccb : CnxnCtxtBranch[Namespace,Var,Tag] => {			
	val xqcc =
	  XQCC(
	    Some( ccb ),
	    ccl,
	    ccRootStr, Some( nextXQV ),
	    None, None,
	    DeBruijnIndex( 0, 0 )
	  )
	xqRecConstraints( ccl, xqcc )
      }
    }    
  }
}




