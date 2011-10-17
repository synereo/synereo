// -*- mode: Scala;-*- 
// Filename:    Transitions.scala 
// Authors:     lgm                                                    
// Creation:    Tue Aug  9 20:35:07 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.seleKt.model.ill.vm.illvm.executive

import com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn._

object SyntaxConversion {
  import scala.collection.JavaConversions._
  
  // convenience
  case class TMState(
    stack : List[Either[Env,Value]],
    env : List[Value],
    code : List[Instruction],
    dump : List[Frame]
  )

  implicit def asTMState( machine : MACHINE ) : TMState = {
    TMState(
      fromStack( machine.stack_ ),
      fromEnv( machine.env_ ),
      fromILLCode( machine.illcode_ ),
      fromDump( machine.dump_ )
    )
  }

  implicit def fromStack( stack : Stack ) : List[Either[Env,Value]] = {
    stack match {
      case mstk : MSTACK => {
	mstk.listenvorval_.toList.map( fromEnvOrVal )
      }
      case _ => throw new Exception( "unexpected stack type" )
    }
  }
  implicit def asStack( eOrVs : List[Either[Env,Value]] ) : Stack = {
    val leov = new ListEnvOrVal()
    leov.addAll( eOrVs.map( asEnvOrVal ) )
    new MSTACK( leov )
  }
  implicit def fromEnv( env : Env ) : List[Value] = {
    env match {
      case e : ENVIRONMENT => {
	e.listvalue_.toList
      }
      case _ => throw new Exception( "unexpected environment type" )
    }
  }
  implicit def asEnv( vals : List[Value] ) : Env = {
    val lvs = new ListValue( )
    lvs.addAll( vals )
    new ENVIRONMENT( lvs ) 
  }  
  implicit def fromILLCode( illcode : ILLCode ) : List[Instruction] = {
    illcode match {
      case codes : CODESEQ => {
	codes.listinstruction_.toList
      }
      case _ => throw new Exception( "unexpected machine codes type" )
    }
  }
  implicit def asILLCode( code : List[Instruction] ) : ILLCode = {
    val lis = new ListInstruction( )
    lis.addAll( code )
    new CODESEQ( lis )
  }
  implicit def fromDump( dump : Dump ) : List[Frame] = {
    dump match {
      case mdmp : MDUMP => {
	mdmp.listframe_.toList
      }
      case _ => throw new Exception( "unexpected dump type" )
    }
  }
  implicit def asDump( frames : List[Frame] ) : Dump = {
    val lif = new ListFrame( )
    lif.addAll( frames )
    new MDUMP( lif )
  }
  implicit def fromStackEnv( stkEnv : STACKENV ) : List[Value] = {
    fromEnv( stkEnv.env_ )
  }
  implicit def fromStackVal( stkVal : STACKVAL ) : Value = {
    stkVal.value_
  }
  implicit def fromEnvOrVal( eOrV : EnvOrVal ) : Either[Env,Value] = {
    eOrV match {
      case stkEnv : STACKENV => Left[Env,Value]( stkEnv.env_ )
      case stkVal : STACKVAL => Right[Env,Value]( stkVal.value_ )
      case _ => throw new Exception( "unexpected stack element type" )
    }
  }
  implicit def asEnvOrVal( eOrV : Either[Env,Value] ) = {
    eOrV match {
      case Left( e ) => new STACKENV( e )
      case Right( v ) => new STACKVAL( v )
    }
  }
  implicit def fromFrame(
    frame : Frame
  ) : ( List[Either[Env,Value]], List[Value], List[Instruction] ) = {
    frame match {
      case stkFrame : STACKFRAME => {
	(
	  fromStack( stkFrame.stack_ ),
	  fromEnv( stkFrame.env_ ),
	  fromILLCode( stkFrame.illcode_ )
	)
      }
    }
  }
  implicit def asFrame(
    stack : List[Either[Env,Value]],
    env : List[Value],
    code : List[Instruction]
  ) : Frame = {
    new STACKFRAME(
      asStack( stack ),
      asEnv( env ),
      asILLCode( code )
    )
  }

  implicit def asMachine( tmstate : TMState ) : MACHINE = {
    new MACHINE( 
      tmstate.stack,
      tmstate.env,
      tmstate.code,
      tmstate.dump
    )
  }    
}
