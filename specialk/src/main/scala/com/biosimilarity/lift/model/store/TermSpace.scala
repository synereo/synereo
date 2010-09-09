// -*- mode: Scala;-*- 
// Filename:    TermSpace.scala 
// Authors:     lgm                                                    
// Creation:    Wed Aug 25 16:37:31 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.lib._

import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import org.prolog4j._

import java.net.URI

trait TermTypes[Namespace,Var,Tag,Value] {
  trait Resource
  case class Ground( v : Value ) extends Resource
  case class RMap(
    m : TMap[Namespace,Var,Tag,Value]
  ) extends Resource
  case class RBound(
    rsrc : Option[Resource], soln : Option[Solution[String]]
  ) extends Resource

  type WhatNext = Option[Resource] => Option[Resource]
  type GetContinuation = Option[Resource] => Option[Resource]
  type GetRequest = CnxnCtxtLabel[Namespace,Var,Tag]  

  class TMap[Namespace,Var,Tag,Value]
  extends HashMap[GetRequest,Resource]

  trait IOParity
  case object Input extends IOParity
  case object Output extends IOParity
}

class TermMap[Namespace,Var,Tag,Value]
extends HashMap[
  CnxnCtxtLabel[Namespace,Var,Tag],
  Either[TermMap[Namespace,Var,Tag,Value],Value]
]

trait TermSpace[Namespace,Var,Tag,Value]
extends TermTypes[Namespace,Var,Tag,Value] {
  self : CnxnCtxtInjector[Namespace,Var,Tag] with Journalist with WireTap =>

  val reportage = report( Twitterer() ) _
  
  def get(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    next : WhatNext
  ) : Seq[Option[Resource]]
  def get(
    path : CnxnLabel[Namespace,Tag],
    next : WhatNext
  ) : Seq[Option[Resource]] = {
    get( injectLabel( path ), next )
  }

  def get(
    path : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : Seq[Option[Resource]] = {
    get( path, ( v : Option[Resource] ) => { tap( v ); v } )
  }
  def get(
    path : CnxnLabel[Namespace,Tag]
  ) : Seq[Option[Resource]] = {
    get( injectLabel( path ) )
  }

  def put(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    resource : Resource
  ) : Unit
  def put(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) : Unit
  def put(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    pmap : TMap[Namespace,Var,Tag,Value]
  ) : Unit

  def put(
    path : CnxnLabel[Namespace,Tag],
    resource : Resource
  ) : Unit = {
    put( injectLabel( path ), resource )
  }
  def put(
    path : CnxnLabel[Namespace,Tag],
    value : Value
  ) : Unit = {
    put( injectLabel( path ), value )
  }
  def put(
    path : CnxnLabel[Namespace,Tag],
    pmap : TMap[Namespace,Var,Tag,Value]
  ) : Unit = {
    put( injectLabel( path ), pmap )
  }
}

class TermStore[Namespace,Var,Tag,Value](
)
extends TermSpace[Namespace,Var,Tag,Value] 
with CnxnCtxtInjector[Namespace,Var,Tag]
with CnxnUnificationQuery[Namespace,Var,Tag]
with CnxnQuery[Namespace,Var,Tag]
with StructuredPrologQuery[Namespace,Var,Tag]
with WireTap
with Journalist
with UUIDOps {
  lazy val _labelMap = new TMap[Namespace,Var,Tag,Value]()
  lazy val _waiters = new HashMap[GetRequest,List[GetContinuation]]()  

  override def tap [A] ( fact : A ) : Unit = {
    reportage( fact )
  }

  def places(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    parity : IOParity
  ) : Seq[(CnxnCtxtLabel[Namespace,Var,Tag], Option[Solution[String]])]
  = {
    (parity match {
      case Input => _labelMap
      case Output => _waiters
    }).keys.toList.flatMap( 
      { 
	( lbl ) => {
	  matches( path, lbl ) match {
	    case None => List()
	    case soln @_ => {
	      List( ( lbl, soln ) )
	    }
	  }
	}
      }
    ) match {
      case Nil => List( ( path, None ) )
      case ps @ _ => ps
    }
  }

  override def get(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    next : WhatNext
  )
  : Seq[Option[Resource]] = {        
    for( ( place, soln ) <- places( path, Input ) )
    yield {
      _labelMap.get( place ) match {
	case sv @ Some( value ) => {
	  _labelMap -= place
	  sv
	}
	case None => {
	  reset {
	    val rslt : Option[Resource] = 
	      shift {
		( k : GetContinuation ) => {	      
		  _waiters( place ) =
		    _waiters.get( place ).getOrElse( Nil ) ++ List( k )
		  
		  k( None )
		}	    	      
	      }
	    reportage( "resuming with value : " + rslt )
	    rslt match {
	      case Some( _ ) =>	next( rslt )
	      case nv @ _ => nv
	    }
	  }
	}	
      }
    }    
  }  

  override def put(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    resource : Resource
  ) : Unit = {    
    for( ( place, soln ) <- places( path, Output ) ) {
      _waiters.get( place ) match {
	case Some( k :: ks ) => {
	  _waiters( place ) = ks
	  k( Some( RBound( Some( resource ), soln ) ) )
	}
	case None => {
	  _labelMap( place ) = resource
	}	
      }
    }
  }

  override def put(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) : Unit = {
    put( path, Ground( value ) )
  }

  override def put(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    pmap : TMap[Namespace,Var,Tag,Value]
  ) : Unit = {
    put( path, RMap( pmap ) )
  }
}
