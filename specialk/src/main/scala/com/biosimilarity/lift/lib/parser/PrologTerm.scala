// -*- mode: Scala;-*- 
// Filename:    PrologTerm.scala 
// Authors:     lgm                                                    
// Creation:    Wed Oct 26 04:32:12 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.parsing.prolog.term

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
import com.biosimilarity.lift.model.store.CnxnXQuery

import scala.util.parsing.combinator._

import java.io.StringReader

trait PrologTermParsing {
  // Concrete syntax to abstract syntax xform
  // Validation
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

    // Transformation
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

trait CaseClassConversion {
  val stdCaseClassMethods =
    List[String]( 
      "equals",
      "toString",
      "hashCode",
      "copy",
      "productPrefix",
      "productArity",
      "productElement",
      "productIterator",
      "productElements",
      "canEqual",
      "copy$default$1",
      "wait",
      "wait",
      "wait",
      "getClass",
      "notify",
      "notifyAll"
      )

  def isCaseClass( candidate : Any ) = false
  def isCaseClass( candidate : ScalaObject with Product with Serializable ) = true

  def toTermSymbol( msg : ScalaObject with Product with Serializable ) : String = {
    val msgClassName = msg.getClass.getName
    val msgName =
      msgClassName.substring( 
	msgClassName.indexOf( "$" ) + 1,
	msgClassName.length 
      )
    
    msgName.take( 1 ).toLowerCase + msgName.drop( 1 )
  }
  
  def toTermActuals( msg : ScalaObject with Product with Serializable ) : String = {
    def getArg( msg : ScalaObject with Product with Serializable, mthd : String ) : String = {
      val meth = msg.getClass.getMethod( mthd )
      val mbr = meth.invoke( msg )
      if ( isCaseClass( mbr ) ) {
	toTerm( mbr.asInstanceOf[ScalaObject with Product with Serializable] )
      }
      else {
	mbr.toString
      }
    }

    val methods = msg.getClass.getMethods.toList

    methods match {
      case mthd :: mthds => {
	( getArg( msg, mthd.getName ) /: mthds )(
	  ( acc, m ) => {
	    val methName = m.getName
	    if (
	      stdCaseClassMethods.contains( methName )
	      || methName.contains( "$$$outer" )
	      || methName.contains( "copy$default" )
	    ) {
	      acc
	    }
	    else {
	      acc + "," + getArg( msg, methName )
	    }
	  }
	)
      }
      case Nil => " "
    }    
  }

  def toTerm( msg : ScalaObject with Product with Serializable ) : String = {
    (
      toTermSymbol( msg )
      + "("
      + toTermActuals( msg )
      + ")"
    )
  }
}
