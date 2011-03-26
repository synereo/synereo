// -*- mode: Scala;-*- 
// Filename:    generators.scala<2> 
// Authors:     lgm                                                    
// Creation:    Sun Jan 23 20:50:44 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.agent
import com.biosimilarity.lift.lib.zipper._

import scala.collection.SeqProxy

//import scala.collection.mutable._

trait NormalProcesses {
  type Name <: { def /( substitutions : ( Name, Name )* ) : Name }
  type Agent <: { def /( substitutions : ( Name, Name )* ) : Agent }
  // type EQ <: {
//     def sEq( a1 : Agent, a2 : Agent ) : Boolean; 
//     def aEq( a1 : Agent, a2 : Agent ) : Boolean;
//     def nEq( n1 : Name, n2 : Name ) : Boolean
//   }
  
  // M,N ::= 0
  //         xA
  //         switch { case ptn1 => M1 ... case ptnk => Mk }

  type Presence = ( Name, Agent )

  trait NormalProcess   
  extends Tree[Presence]
  with SeqProxy[Either[Presence,NormalProcess]] {
    def /( substitions : ( Name, Name )* ) : NormalProcess
    override def toString() : String = {
      this match {
	case Zero => "{}"
	case LocatedAgent( x, a ) => x.toString + a.toString
	case Sum( summands ) => {
	   summands match {
	     case Nil => "{}"
	     case s :: rsum => {
	       ( ( s + "" ) /: rsum )( 
		 { 
		   ( acc, sand ) => {
		     acc + " + " + sand
		   }
		 }
	       )
	     }
	   }
	 }
      }
    }
  }

  case object Zero extends NormalProcess {
    def self = Nil
    def /( substitions : ( Name, Name )* ) : NormalProcess = Zero
    override def toString() : String = "{}"
  }
  case class LocatedAgent( x : Name, a : Agent )
       extends NormalProcess {
	 def self = List( Left( ( x, a ) ) )
	 override def /( substitions : ( Name, Name )* ) : NormalProcess = {
	   // TBD
	   LocatedAgent( x, a /( substitions:_* ) )
	 }
	 override def toString() : String = x.toString + a.toString
       }
  case class Sum( summands : List[NormalProcess] )
       extends NormalProcess {
	 def self = summands.map( Right( _ ) )
	 override def /( substitions : ( Name, Name )* ) : NormalProcess = {
	   // TBD
	   Sum( summands.map( _ /( substitions:_* ) ) )
	 }
	 override def toString() : String = {
	   summands match {
	     case Nil => "{}"
	     case s :: rsum => {
	       ( ( s + "" ) /: rsum )( 
		 { 
		   ( acc, sand ) => {
		     acc + " + " + sand
		   }
		 }
	       )
	     }
	   }
	 }
       }

  def locate( x : Name, a : Agent ) : NormalProcess =
    LocatedAgent( x, a )  
  def zero : NormalProcess = Zero
}

trait Agents {
  type Name <: { def /( substitutions : ( Name, Name )* ) : Name }
  type Process <: { def /( substitutions : ( Name, Name )* ) : Process }

  // type EQ <: {
//     def sEq( a1 : Process, a2 : Process ) : Boolean; 
//     def aEq( a1 : Process, a2 : Process ) : Boolean;
//     def nEq( n1 : Name, n2 : Name ) : Boolean
//   }

  // A ::= ?(x0,...,xN)P
  //       !(P0,...,PN)  

  trait Agent {
    def /( substitions : ( Name, Name )* ) : Agent
    override def toString() : String = {
      this match {
	case Abstraction( formals, body ) => {
	   "?" + formals.toString.replace( "List", "" ) + "=>" + body
	 }
	case SemanticAbstraction( abs ) => {
	  "?" + abs
	}
	case Concretion( actuals ) => {
	  actuals match {
	    case Nil => "!" + "()"
	    case a :: ractls => {
	      ( ( a.toString ) /: ractls )(
		{
		  ( acc, actl ) => {
		    acc + ", " + actl
		  }
		}
	      )
	    }
	  }
	}
      }
    }
  }
  case class Abstraction( formals : List[Name], body: Process )
       extends Agent
       with Function1[List[Name],Process] {
	 override def apply( names : List[Name] ) = {
	   body /(( formals.zip( names ) ):_*)
	 }
	 override def /( substitions : ( Name, Name )* ) : Agent = {
	   // TBD
	   this
	 }
	 override def toString() : String = {
	   "?" + formals.toString.replace( "List", "" ) + "=>" + body
	 }
       }
  case class SemanticAbstraction( abs : ( List[Name] => Process ) )
       extends Agent {
	 override def /( substitions : ( Name, Name )* ) : Agent = {
	   // TBD
	   this
	 }
	 override def toString() : String = "?" + abs
       }
  case class Concretion( actuals : List[Process] )
       extends Agent {
	 override def /( substitions : ( Name, Name )* ) : Agent = {
	   // TBD
	   this
	 }
	 override def toString() : String = {
	   actuals match {
	     case Nil => "!" + "()"
	     case a :: ractls => {
	       val actlStr =
		 ( ( a.toString ) /: ractls )(
		   {
		     ( acc, actl ) => {
		       acc + ", " + actl
		     }
		   }
		 )
	       "!" + "( " + actlStr + " )" 
	     }
	   }
	 }
       }

  def abs( formals : List[Name], body : Process ) : Agent = 
    Abstraction( formals, body )
  def abs( abstraction : ( List[Name] => Process ) ) : Agent = 
    SemanticAbstraction( abstraction )
  def pack( actuals : List[Process] ) : Agent =
    Concretion( actuals )
}

trait Processes {
  type Name <: { def /( substitutions : ( Name, Name )* ) : Name }
  type NormalProcess <: { def /( substitutions : ( Name, Name )* ) : NormalProcess }

  // type EQ <: {
//     def sEq( a1 : NormalProcess, a2 : NormalProcess ) : Boolean; 
//     def aEq( a1 : NormalProcess, a2 : NormalProcess ) : Boolean;
//     def nEq( n1 : Name, n2 : Name ) : Boolean
//   }

  // P,Q ::= N
  //         P|Q
  //         *x

  type Substance = Either[Deref,NormalProcess]
  trait Process
  extends Tree[Process]
  with SeqProxy[Either[Substance,Process]] {
    def /( substitutions : ( Name, Name )* ) : Process
    override def toString() : String = {
      this match {
	case Tangle( n ) => {
	   n + ""
	 }
	case Par( parands ) => {
	   parands match {
	     case Nil => "{}"
	     case p :: rpar => {
	       ( ( p.toString ) /: rpar )(
		 {
		   ( acc, parand ) => {
		     acc + " | " + parand
		   }
		 }
	       )
	     }
	   }
	 }
	case Deref( x ) => {
	   "*" + x
	 }
      }
    }
  }

  case class Tangle( n : NormalProcess )
       extends Process {
	 def self = List( Left( Right( n ) ) )
	 override def /( substitions : ( Name, Name )* ) : Process = {
	   // TBD
	   this
	 }
	 override def toString() : String = {
	   n + ""
	 }
       }
  case class Par( parands : List[Process] )
       extends Process {
	 def self = parands.map( Right( _ ) )
	 override def /( substitions : ( Name, Name )* ) : Process = {
	   // TBD
	   this
	 }
	 override def toString() : String = {
	   parands match {
	     case Nil => "{}"
	     case p :: rpar => {
	       ( ( p.toString ) /: rpar )(
		 {
		   ( acc, parand ) => {
		     acc + " | " + parand
		   }
		 }
	       )
	     }
	   }
	 }
       }
  case class Deref( x : Name )
       extends Process {
	 def self = List( Left( Left( this ) ) )
	 override def /( substitions : ( Name, Name )* ) : Process = {
	   // TBD
	   this
	 }
	 override def toString() : String = {
	   "*" + x
	 }
       }

  def par( parands : List[Process] ) : Process =
    Par( parands )
  def deref( x : Name ) : Process = Deref( x )
  def *( x : Name ) : Process =
    deref( x )
}

trait Names {
  type Agent <: { def /( substitutions : ( Name, Name )* ) : Agent }
  type Process <: { def /( substitutions : ( Name, Name )* ) : Process }
  type NormalProcess <: { def /( substitutions : ( Name, Name )* ) : NormalProcess }

  // type EQ <: {
//     def sEq( a1 : Process, a2 : Process ) : Boolean; 
//     def sEq( a1 : Agent, a2 : Agent ) : Boolean; 
//     def sEq( a1 : NormalProcess, a2 : NormalProcess ) : Boolean; 
//     def aEq( a1 : Process, a2 : Process ) : Boolean;
//     def aEq( a1 : Agent, a2 : Agent ) : Boolean; 
//     def aEq( a1 : NormalProcess, a2 : NormalProcess ) : Boolean; 
//   }

  // x,y ::= <<P>>

  def locate( name : Name, code : Agent ) : NormalProcess
  def locate( name : Name, abstraction : ( List[Name] => Process ) ) : NormalProcess
  def abs( formals : List[Name], body : Process ) : Agent
  def abs( abstraction : ( List[Name] => Process ) ) : Agent
  def pack( actuals : List[Process] ) : Agent

  case class Name( code : Process )
  {
    def /( substitutions : ( Name, Name )* ) : Name = {
      // TBD
      this
    }
    def ?( abstraction : ( List[Name] => Process ) ) : NormalProcess = {
      locate( this, abstraction )
    }
    def ?( formals : Name* )( body : Process ) : NormalProcess = {
      locate( this, abs( formals.toList, body ) )
    }
    def !( actuals : Process* ) : NormalProcess = {
      locate( this, pack( actuals.toList ) )
    }    
    override def toString() : String = {
      "@" + "<" + code + ">"
    }
  }

  def quote( process : Process ) : Name =
    Name( process )
  def `@`( process : Process ) : Name =
    quote( process )
  
}

trait Generators
 extends Names
       with NormalProcesses
       with Agents
       with Processes
{
  val TZ : Process = Tangle( Zero )
  val NZ : Name = quote( TZ )  
  
  override def locate(
    name : Name,
    abstraction : ( List[Name] => Process )
  ) : NormalProcess = {
    LocatedAgent( name, SemanticAbstraction( abstraction ) )
  }

  override def par( parands : List[Process] ) : Process =
    Par( parands )
  override def *( x : Name ) : Process =
    deref( x )
  override def quote( process : Process ) : Name =
    Name( process )  
  override def `@`( process : Process ) : Name =
    quote( process )
}
