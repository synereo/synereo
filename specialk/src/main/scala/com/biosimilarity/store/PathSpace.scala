// -*- mode: Scala;-*- 
// Filename:    PathSpace.scala 
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

class PathMap[Namespace,Tag,Value]
extends HashMap[
  CnxnPath[Namespace,Tag],
  Either[PathMap[Namespace,Tag,Value],Value]
]

trait PathSpace[Namespace,Tag,Value] {
  type Resource = Either[PathMap[Namespace,Tag,Value],Value]
  type WhatNext = Option[Resource] => Option[Resource]
  type GetContinuation = Option[Resource] => Option[Resource]
  type GetRequest = CnxnPath[Namespace,Tag]  
  
  def get(
    path : CnxnPath[Namespace,Tag],
    next : WhatNext
  ) : Option[Resource]
  def put(
    path : CnxnPath[Namespace,Tag],
    resource : Resource
  ) : Unit
  def put(
    path : CnxnPath[Namespace,Tag],
    value : Value
  ) : Unit
  def put(
    path : CnxnPath[Namespace,Tag],
    pmap : PathMap[Namespace,Tag,Value]
  ) : Unit
}

class PathStore[Namespace,Tag,Value](
  val verbosity : Verbosity
)
extends PathSpace[Namespace,Tag,Value] {
  lazy val _pathMap = new PathMap[Namespace,Tag,Value]()
  lazy val _pathCache = new HashMap[GetContinuation,Resource]()
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
    path : CnxnPath[Namespace,Tag],
    next : WhatNext
  )
  : Option[Resource] = {
    _pathMap.get( path ) match {
      case None => {
	reset {
	  val rslt : Option[Resource] = 
	    shift {
	      ( k : GetContinuation ) => {	      
		_waiters( path ) =
		  _waiters.get( path ).getOrElse( Nil ) ++ List( k )
		report( "pivot point" )
		k( _pathCache.get( k ) )
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
	_pathMap - path
	sv
      }
    }
  }
  override def put(
    path : CnxnPath[Namespace,Tag],
    resource : Resource
  ) : Unit = {
    _waiters.get( path ) match {
      case None => {
	_pathMap( path ) = resource
      }
      case Some( waiters ) => {
	for( k <- waiters ) {
	  _pathCache( k ) = resource; k ( Some( resource ) )
	}
	_waiters( path ) = Nil
      }
    }
  }
  override def put(
    path : CnxnPath[Namespace,Tag],
    value : Value
  ) : Unit = {
    put( path, Right( value ) )
  }
  override def put(
    path : CnxnPath[Namespace,Tag],
    pmap : PathMap[Namespace,Tag,Value]
  ) : Unit = {
    put( path, Left( pmap ) )
  }
}
