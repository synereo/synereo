// -*- mode: Scala;-*- 
// Filename:    REPL.scala 
// Authors:     lgm                                                    
// Creation:    Thu May  8 10:18:48 2008 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model

import com.biosimilarity.lift.model.specialK._

import Absyn._
import Eval._
//import Compile._

import scala.collection.immutable.HashMap

import java.net.URI
import java.util.UUID
import java.io.StringReader

class REPL {
  // parsing
  def lexer (str : String) = new Yylex( new StringReader( str ) )
  def parser (str : String) = new parser( lexer( str ) )
  def clientRequestParseTree (str : String) = (parser( str )).pAgent()
  //def clientRequestParseTree (str : String) = (parser( str )).pLGrammar()
  //def clientRequestParseTree (str : String) = (parser( str )).pExpression()
  def read (str : String) = clientRequestParseTree(str)  

  // compilation    
  object theCompiler extends Compiler {
    def compile( expr : Agent ) = {
      toScalaSource(
	expr, 
	new scala.collection.mutable.HashMap[Agent,UUID](),
	new scala.collection.mutable.HashMap[Variation,(Agent,Option[Source])]()
      )
    }
  }
  def compile( str : String ) = {
    theCompiler.compile( read( str ) )
  }
  
  // evaluation  

  // printing
  def showClientRequestParseTree (str : String) =
    PrettyPrinter.show(clientRequestParseTree(str))    
}
