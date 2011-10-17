// -*- mode: Scala;-*- 
// Filename:    REPL.scala 
// Authors:     lgm                                                    
// Creation:    Thu May  8 10:18:48 2008 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._
import com.biosimilarity.seleKt.model.ill.lang.illtl._
import com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn._
import com.biosimilarity.seleKt.model.ill.compiler._
import com.biosimilarity.seleKt.model.ill.vm.illvm.executive._

import scala.collection.immutable.HashMap
import scala.util.parsing.combinator._

import java.net.URI
import java.util.UUID
import java.io.StringReader

class RLLREPL {
  import SyntaxConversion._
  // parsing
  def lexer (str : String) = new Yylex( new StringReader( str ) )
  def parser (str : String) = new parser( lexer( str ) )
  def clientRequestParseTree (str : String) = (parser( str )).pRLLExpr()
  def read (str : String) = clientRequestParseTree(str)  

  // compilation      
  def compile( str : String ) = {
    CompilerNoCtxt.compile( 
      read( str ),
      Nil,
      ()
    )
  }
  
  // evaluation  
  def eval( str : String ) = {    
    val ( code, ctxt ) = compile( str )
    val tmstate = 
      TMState(
	Nil,
	Nil,
	code,
	Nil
      )
    val ( v, _ ) =
      TraceTransitions.reduce( tmstate, VMReadBack )
    com.biosimilarity.seleKt.model.ill.vm.illvm.PrettyPrinter.show( v )
  }  

  // printing
  def showClientRequestParseTree (str : String) =
    PrettyPrinter.show(clientRequestParseTree(str))    

  // Concrete syntax to abstract syntax xform
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



/*
Welcome to Scala version 2.9.0.1 (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_26).
Type in expressions to have them evaluated.
Type :help for more information.

scala> import com.biosimilarity.lift.model._
import com.biosimilarity.lift.model._

scala> import com.biosimilarity.seleKt.model.ill.vm.illvm.executive._
import com.biosimilarity.seleKt.model.ill.vm.illvm.executive._

scala> import com.biosimilarity.seleKt.model.ill.compiler._
import com.biosimilarity.seleKt.model.ill.compiler._

scala> import com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn._
import com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn._

scala> import com.biosimilarity.seleKt.model.ill.lang.illtl._
import com.biosimilarity.seleKt.model.ill.lang.illtl._

scala> import java.io.StringReader
import java.io.StringReader

scala> import SyntaxConversion._
import SyntaxConversion._

scala> val repl1 = new RLLREPL()
repl1: com.biosimilarity.lift.model.RLLREPL = com.biosimilarity.lift.model.RLLREPL@4c8eb7f9

scala> repl1.eval( "(lambda u.(let u be x (x) y in (<x,y>))) (* (x) *);" )
 stack: 
  []
 env: 
  []
 code: 
  [UNIT:UNIT:PAIR:PUSH:MAKEFCL(#<code>):AP]
 dump: 
  []

 ==========> 
 stack: 
  [*]
 env: 
  []
 code: 
  [UNIT:PAIR:PUSH:MAKEFCL(#<code>):AP]
 dump: 
  []

 ==========> 
 stack: 
  [*:*]
 env: 
  []
 code: 
  [PAIR:PUSH:MAKEFCL(#<code>):AP]
 dump: 
  []

 ==========> 
 stack: 
  [(*,*)]
 env: 
  []
 code: 
  [PUSH:MAKEFCL(#<code>):AP]
 dump: 
  []

 ==========> 
 stack: 
  []
 env: 
  [(*,*)]
 code: 
  [MAKEFCL(#<code>):AP]
 dump: 
  []

 ==========> 
 stack: 
  [fcl(#<code>,#<env>)]
 env: 
  [(*,*)]
 code: 
  [AP]
 dump: 
  []

 ==========> 
 stack: 
  []
 env: 
  [(*,*):(*,*)]
 code: 
  [PUSHENV:HD:UNPAIR:PUSH:PUSH:MAKECCL(#<code>,#<code>):POP:RET]
 dump: 
  [[#<stack>,#<env>,#<code>]]

 ==========> 
 stack: 
  [[(*,*):(*,*)]]
 env: 
  [(*,*):(*,*)]
 code: 
  [HD:UNPAIR:PUSH:PUSH:MAKECCL(#<code>,#<code>):POP:RET]
 dump: 
  [[#<stack>,#<env>,#<code>]]

 ==========> 
 stack: 
  [(*,*):[(*,*):(*,*)]]
 env: 
  [(*,*):(*,*)]
 code: 
  [UNPAIR:PUSH:PUSH:MAKECCL(#<code>,#<code>):POP:RET]
 dump: 
  [[#<stack>,#<env>,#<code>]]

 ==========> 
 stack: 
  [*:*:[(*,*):(*,*)]]
 env: 
  [(*,*):(*,*)]
 code: 
  [PUSH:PUSH:MAKECCL(#<code>,#<code>):POP:RET]
 dump: 
  [[#<stack>,#<env>,#<code>]]

 ==========> 
 stack: 
  [*:[(*,*):(*,*)]]
 env: 
  [*:(*,*):(*,*)]
 code: 
  [PUSH:MAKECCL(#<code>,#<code>):POP:RET]
 dump: 
  [[#<stack>,#<env>,#<code>]]

 ==========> 
 stack: 
  [[(*,*):(*,*)]]
 env: 
  [*:*:(*,*):(*,*)]
 code: 
  [MAKECCL(#<code>,#<code>):POP:RET]
 dump: 
  [[#<stack>,#<env>,#<code>]]

 ==========> 
 stack: 
  [ccl(#<code>,#<code>,#<env>):[(*,*):(*,*)]]
 env: 
  [*:*:(*,*):(*,*)]
 code: 
  [POP:RET]
 dump: 
  [[#<stack>,#<env>,#<code>]]

 ==========> 
 stack: 
  [ccl(#<code>,#<code>,#<env>):[(*,*):(*,*)]]
 env: 
  [*:(*,*):(*,*)]
 code: 
  [RET]
 dump: 
  [[#<stack>,#<env>,#<code>]]

 ==========> 
res0: java.lang.String = "(CclV (CODESEQ [(PUSHENV "PUSHENV"), (HEAD "HD"), (RET "RET")]) (CODESEQ [(PUSHENV "PUSHENV"), (TAIL "TL"), (HEAD "HD"), (RET "RET")]) (ENVIRONMENT [UnitV, UnitV, (PairV UnitV UnitV), (PairV UnitV UnitV)])) "

scala> 
*/
