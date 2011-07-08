// -*- mode: Scala;-*- 
// Filename:    REPL.scala 
// Authors:     lgm                                                    
// Creation:    Thu May  8 10:18:48 2008 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model

import com.biosimilarity.lift.lib.game.conway._
import com.biosimilarity.lift.lib.collection.{ DeCantor => Set, _ }

import com.biosimilarity.lift.model.specialK._

import Absyn._
import Eval._
//import Compile._

import scala.collection.immutable.HashMap
import scala.collection.SeqProxy

import java.net.URI
import java.util.UUID
import java.io.StringReader

class ConwayCalcREPL {
  val expr =
    new scala.collection.mutable.HashMap[String,List[String]]()

  def addToExpr( exprKey : String, fragment : String ) = {
    expr += ( ( exprKey, expr.getOrElse( exprKey, Nil ) ++ List( fragment ) ) )
  }

  case class SList(
    override val self : List[Either[String,SList]]
  ) extends SeqProxy[Either[String,SList]]

  def parse( exprKey : String ) : SList = {
    def loop(
      acc : ( String, SList ),
      sl : List[String]
    ) : (String, SList ) = {      
      def dispatch(
	c : String,
	l : SList,
	slp : List[String],
	op : String
      ) : ( String, SList ) = {
	if ( c == "" ) {
	  loop( ( c, l ), slp )
	} 
	else {
	  loop( ( "", SList( Nil ) ), slp ) match {
	    case ( "", nslp ) => {
	      (
		"",
		SList(
		  List(
		    Left( op ),
		    Left( c ),
		    Right( nslp )
		  )
		)
	      )
	    }
	    case ( cp, slpp ) => {
	      val innerExpr = 
		SList( List( Left( "+" ), Left( c ), Left( cp ) ) )
	      if ( slpp.isEmpty ) {
		( "", innerExpr )
	      }
	      else {
		(
		  "",
		  SList(
		    List(
		      Left( "+" ),
		      Right( innerExpr ),
		      Right( slpp )
		    )
		  )
		)
	      }
	    }
	  }		
	}
      }
      sl match {
	case Nil => {
	  acc
	}
	case e :: slp => {
	  val ( c, l ) = acc
	  e match {
	    case "0" => { loop( ( c ++ e, l ), slp ) }
	    case "1" => { loop( ( c ++ e, l ), slp ) }
	    case "2" => { loop( ( c ++ e, l ), slp ) }
	    case "3" => { loop( ( c ++ e, l ), slp ) }
	    case "4" => { loop( ( c ++ e, l ), slp ) }
	    case "5" => { loop( ( c ++ e, l ), slp ) }
	    case "6" => { loop( ( c ++ e, l ), slp ) }
	    case "7" => { loop( ( c ++ e, l ), slp ) }
	    case "8" => { loop( ( c ++ e, l ), slp ) }
	    case "9" => { loop( ( c ++ e, l ), slp ) }
	    case "+" => {
	      dispatch( c, l, slp, "+" )
	    }
	    case "-" => {
	      dispatch( c, l, slp, "-" )
	    }
	    case "x" => {
	      dispatch( c, l, slp, "x" )
	    }
	    case "/" => {
	      dispatch( c, l, slp, "/" )
	    }
	    case "=" => {
	      if ( c == "" ) {
		loop( ( c, l ), slp )
	      } 
	      else {
		loop( ( "", SList( List( Left( c ) ) ) ), slp )
	      }
	    }
	    case "c" => {
	      loop( ( "", SList( Nil ) ), slp )
	    }
	  }
	}
      }
    }
    val ( t, s ) =
      loop( ( "", SList( Nil ) ), expr.getOrElse( exprKey, Nil ) );
    s
  }

  object acalc extends ConwayCalculator

  def eval( sl : SList ) : ( Double, ConwayGame ) = {
    sl match {
      case SList( Nil ) => { ( 0, EmptyGame ) }
      case SList( Left( n ) :: Nil ) => {
	val nd = n.toInt
	( nd, acalc.toConwayGame( nd ) )
      }
      case SList( Left( "+" ) :: Left( n ) :: Right( nsl ) :: Nil ) => {
	val nd = n.toInt
	val nc = acalc.toConwayGame( nd )
	val ( rd, rc ) = eval( nsl )
	( nd + rd, acalc.add( nc, rc ) )
      }
      case SList( Left( "-" ) :: Left( n ) :: Right( nsl ) :: Nil ) => {
	val nd = n.toInt
	val nc = acalc.toConwayGame( nd )
	val ( rd, rc ) = eval( nsl )
	( nd - rd, acalc.add( nc, acalc.minus( rc ) ) )
      }
      case SList( Left( "-" ) :: Left( n ) :: Nil ) => {
	val nd = n.toInt
	val nc = acalc.toConwayGame( nd )
	( - nd, acalc.minus( nc ) )
      }
      case SList( Left( "x" ) :: Left( n ) :: Right( nsl ) :: Nil ) => {
	val nd = n.toInt
	val nc = acalc.toConwayGame( nd )
	val ( rd, rc ) = eval( nsl )
	( nd * rd, acalc.multiply( nc, rc ) )
      }
      case SList( Left( "/" ) :: Left( n ) :: Right( nsl ) :: Nil ) => {
	throw new Exception( "division not yet implemented" )
      }
      case SList( Left( "eval" ) :: Left( n ) :: Right( SList( Nil ) ) :: Nil ) => {
	val nd = n.toInt
	val nc = acalc.toConwayGame( nd )
	( nd, nc )
      }
    }
  }

  // parsing
  def lexer (str : String) = new Yylex( new StringReader( str ) )
  def parser (str : String) = new parser( lexer( str ) )
  def clientRequestParseTree (str : String) = (parser( str )).pAgent()
  //def clientRequestParseTree (str : String) = (parser( str )).pLGrammar()
  //def clientRequestParseTree (str : String) = (parser( str )).pExpression()
  def read (str : String) = clientRequestParseTree(str)  

  // evaluation  

  // printing
  def showClientRequestParseTree (str : String) =
    PrettyPrinter.show(clientRequestParseTree(str))    
}
