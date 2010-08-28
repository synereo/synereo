// -*- mode: Scala;-*- 
// Filename:    TermSpace.scala 
// Authors:     lgm                                                    
// Creation:    Wed Aug 25 16:37:31 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.store

import com.biosimilarity.lib._

import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import java.net.URI

class TermMap[Namespace,Var,Tag,Value]
extends HashMap[
  CnxnCtxtLabel[Namespace,Var,Tag],
  Either[TermMap[Namespace,Var,Tag,Value],Value]
]

trait TermSpace[Namespace,Var,Tag,Value] {
  self : CnxnCtxtInjector[Namespace,Var,Tag] =>
  type Resource = Either[TermMap[Namespace,Var,Tag,Value],Value]
  type WhatNext = Option[Resource] => Option[Resource]
  type GetContinuation = Option[Resource] => Option[Resource]
  type GetRequest = CnxnCtxtLabel[Namespace,Var,Tag]  
  
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
    pmap : TermMap[Namespace,Var,Tag,Value]
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
    pmap : TermMap[Namespace,Var,Tag,Value]
  ) : Unit = {
    put( injectLabel( path ), pmap )
  }
}

class TermStore[Namespace,Var,Tag,Value](
  val verbosity : Verbosity
)
extends TermSpace[Namespace,Var,Tag,Value] 
with CnxnCtxtInjector[Namespace,Var,Tag]
with CnxnUnificationQuery[Namespace,Var,Tag]
with CnxnQuery[Namespace,Var,Tag]
with StructuredPrologQuery[Namespace,Var,Tag]
with UUIDOps {
  lazy val _labelMap = new TermMap[Namespace,Var,Tag,Value]()
  lazy val _labelCache = new HashMap[GetContinuation,Resource]()
  lazy val _waiters = new HashMap[GetRequest,List[GetContinuation]]()

  def report( msg : String ) : Unit = {
    verbosity match {
      case Terse() => {
      }
      case _ => {
	println( msg )
      }
    }
  }
  
  override def get(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    next : WhatNext
  )
  : Seq[Option[Resource]] = {
    val places =
      _labelMap.keys.filter( 
	{ 
	  ( lbl ) => {
	    matches( path, lbl ) match {
	      case None => false
	      case _ => true
	    }
	  }
	}
      ).toList
    for( place <- places )
    yield {
      _labelMap.get( place ) match {
	case None => {
	  reset {
	    val rslt : Option[Resource] = 
	      shift {
		( k : GetContinuation ) => {	      
		  _waiters( place ) =
		    _waiters.get( place ).getOrElse( Nil ) ++ List( k )
		  report( "pivot point" )
		  k( _labelCache.get( k ) )
		}	    	      
	      }
	    report( "pivot back, rslt = " + rslt )
	    rslt match {
	      case Some( v ) => {
		next( rslt )
	      }
	      case None => {
	      }
	    }
	    rslt
	  }
	}
	case sv @ Some( value ) => {
	  _labelMap - place
	  sv
	}
      }
    }
  }
  override def put(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    resource : Resource
  ) : Unit = {
    _waiters.get( path ) match {
      case None => {
	_labelMap( path ) = resource
      }
      case Some( waiters ) => {
	for( k <- waiters ) {
	  _labelCache( k ) = resource; k ( Some( resource ) )
	}
	_waiters( path ) = Nil
      }
    }
  }
  override def put(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) : Unit = {
    put( path, Right( value ) )
  }
  override def put(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    pmap : TermMap[Namespace,Var,Tag,Value]
  ) : Unit = {
    put( path, Left( pmap ) )
  }
}
