// -*- mode: Scala;-*- 
// Filename:    Readback.scala 
// Authors:     lgm                                                    
// Creation:    Thu Aug 11 17:52:19 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.seleKt.model.ill.vm.illvm.executive

import com.biosimilarity.seleKt.model.ill.vm.illvm._
import com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn._

trait ReadBack /*[Ctxt]*/ {
  import SyntaxConversion._
  def prettyFold( s : Seq[Any] )( level : Int ) : String = {
    if ( s.size > 0 ) {
      ( prettyPrintD( s.take( 1 )( 0 ) )( level + 1 ) /: s.drop( 1 ) )(
	{
	  ( acc, v ) => {
	    acc + ":" + ( prettyPrintD( v )( level + 1 ) )
	  }
	}
      )
    }
    else {
      ""
    }
  }
  def prettyPrintD( a : Any )( level : Int ) : String = {
    a match {
      case eOrV : Either[Env,Value] =>
	prettyPrint( eOrV )( level )
      case env : Env => 
	prettyPrint( env )( level )
      case instr : Instruction =>
	prettyPrint( instr )( level )
      case value : Value =>
	prettyPrint( value )( level )
      case frame : Frame => 
	prettyPrint( frame )( level )
      case _ => {
	throw new Exception( "unexpected prettyPrint type" )
      }
    }
  }
  def prettyPrint(
    eOrV : Either[Env,Value]
  )( level : Int ) : String = {
    eOrV match {
      case Left( e ) => prettyPrint( e )( level )
      case Right( v ) => prettyPrint( v )( level ) 
    }
  }
  def prettyPrint( e : Env )( level : Int ) : String = {
    "[" + prettyFold( fromEnv( e ) )( level ) + "]"
  }
  def prettyPrint( instr : Instruction )( level : Int ) : String = {
    instr match {
      case pushenv : PUSHENV => {
	"PUSHENV"
      }
      case hd : HEAD => {
	"HD"
      }
      case tl : TAIL => {
	"TL"
      }
      case ret : RET => {
	"RET"
      }
      case push : PUSH => {
	"PUSH"
      }
      case pop : POP => {
	"POP"
      }
      case makefcl : MAKEFCL => {
	"MAKEFCL(" + "#<code>" + ")"
      }
      case ap : AP => {
	"AP"
      }
      case unit : UNIT => {
	"UNIT"
      }
      case ununit : UNUNIT => {
	"UNUNIT"
      }
      case pair : PAIR => {
	"PAIR"
      }
      case unpair : UNPAIR => {
	"UNPAIR"
      }
      case makeccl : MAKECCL => {
	"MAKECCL(" + "#<code>" + "," + "#<code>" + ")"
      }
      case fst : FST => {
	"FST"
      }
      case snd : SND => {
	"SND"
      }
      case inl : INL => {
	"INL"
      }
      case inr : INR => {
	"INR"
      }
      case makeocl : MAKEOCL => {
	"MAKEOCL(" + "#<code>" + ")"
      }
      case rd : READ => {
	"READ"
      }
      case dup : DUP => {
	"DUP"
      }
    }
  }
  def prettyPrint( v : Value )( level : Int ) : String = {
    v match {
      case unit : UnitV => {
	"*"
      }
      case ocl : OclV => {
	"ocl(" + "#<code>" + "," + "#<env>" + ")"
      }
      case ccl : CclV => {
	"ccl(" + "#<code>" + "," + "#<code>" + "," + "#<env>" + ")"
      }
      case fcl : FclV => {
	"fcl(" + "#<code>" + "," + "#<env>" + ")"
      }
      case inl : InlV => {
	"inl(" + prettyPrint( inl.value_ )( level ) + ")"
      }
      case inr : InrV => {
	"inr(" + prettyPrint( inr.value_ )( level ) + ")"
      }
      case pair : PairV => {
	(
	  "("
	  + prettyPrint( pair.value_1 )( level )
	  + ","
	  + prettyPrint( pair.value_2 )( level )
	  + ")"
	)
      }
    }
  }
  def prettyPrint( v : Frame )( level : Int ) : String = {
    "[" + "#<stack>" + "," + "#<env>" + "," + "#<code>" + "]"
  }
  def prettyPrint( tmstate : TMState )( level : Int ) : String = {
    val indentString = 
      ( "" /: (0 to level) )( { ( acc, l ) => acc + " " } )
    val stackString =
      "[" + prettyFold( tmstate.stack )( level + 1 ) + "]"
    val envString =
      "[" + prettyFold( tmstate.env )( level + 1 ) + "]"
    val codeString =
      "[" + prettyFold( tmstate.code )( level + 1 ) + "]"
    val dumpString =
      "[" + prettyFold( tmstate.dump )( level + 1 ) + "]"
    (
      indentString + "stack: " + "\n"
      + indentString + " " + stackString + "\n"
      + indentString + "env: " + "\n"
      + indentString + " " + envString + "\n"
      + indentString + "code: " + "\n"
      + indentString + " " + codeString + "\n"
      + indentString + "dump: " + "\n"
      + indentString + " " + dumpString + "\n"
    )
  }

  def show( machine : MACHINE ) = {
    PrettyPrinter.show( machine )
  }
}

object VMReadBack extends ReadBack 
