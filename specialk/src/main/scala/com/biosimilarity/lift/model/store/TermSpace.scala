// -*- mode: Scala;-*- 
// Filename:    TermSpace.scala 
// Authors:     lgm                                                    
// Creation:    Wed Aug 25 16:37:31 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.xml._
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
    rsrc : Option[Resource], soln : Option[Solution[Object]]
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
  self : CnxnCtxtInjector[Namespace,Var,Tag] with WireTap =>

  // val reportage = report( Twitterer() ) _
  
  def get(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    next : WhatNext,
    peek : Boolean
  ) : Seq[Option[Resource]]
  def get(
    path : CnxnLabel[Namespace,Tag],
    next : WhatNext,
    peek : Boolean
  ) : Seq[Option[Resource]] = {
    get( injectLabel( path ), next, peek )
  }
  def get(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    next : WhatNext
  ) : Seq[Option[Resource]] = {
    get( path, next, false )
  }
  def get(
    path : CnxnLabel[Namespace,Tag],
    next : WhatNext
  ) : Seq[Option[Resource]] = {
    get( injectLabel( path ), next )
  }

  def fetch(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    next : WhatNext
  ) : Seq[Option[Resource]] = {
    get( path, next, true )
  }
  def fetch(
    path : CnxnLabel[Namespace,Tag],
    next : WhatNext
  ) : Seq[Option[Resource]] = {
    fetch( injectLabel( path ), next )
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

  def fetch(
    path : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : Seq[Option[Resource]] = {
    fetch( path, ( v : Option[Resource] ) => { tap( v ); v } )
  }
  def fetch(
    path : CnxnLabel[Namespace,Tag]
  ) : Seq[Option[Resource]] = {
    fetch( injectLabel( path ) )
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
with BaseXCnxnStorage[Namespace,Var,Tag]
//with CnxnStorage[Namespace,Var,Tag]
with CnxnCtxtInjector[Namespace,Var,Tag]
with CnxnUnificationCompositeTermQuery[Namespace,Var,Tag]
with CnxnConversions[Namespace,Var,Tag]
with CnxnXML[Namespace,Var,Tag]
//with XMLStore
with BaseXXMLStore
with Blobify
with WireTap
with ConfigurationTrampoline
with UUIDOps {
  lazy val _labelMap = new TMap[Namespace,Var,Tag,Value]()
  lazy val _waiters = new HashMap[GetRequest,List[GetContinuation]]()  

  override def tap [A] ( fact : A ) : Unit = {
    BasicLogService.reportage( fact )
  }

  override def configurationDefaults : ConfigurationDefaults = {
    ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
  }

  //override def tmpDirStr : String = CnxnStorageDefaults.tmpDirStr

  def places(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    parity : IOParity
  ) : Seq[(CnxnCtxtLabel[Namespace,Var,Tag], Option[Solution[Object]])]
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
    next : WhatNext,
    peek : Boolean
  )
  : Seq[Option[Resource]] = {        
    for( placeNSoln <- places( path, Input ) )
    yield {
      val ( place, soln ) = placeNSoln
      _labelMap.get( place ) match {
	case sv @ Some( value ) => {
	  if ( !peek ) {
	    _labelMap -= place
	  }
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
	    BasicLogService.reportage(
	      (
		this
		+ "resuming with value : "
		+ rslt
	      )
	    )
	    if ( !peek ) {
	      _labelMap -= place
	    }
	    rslt match {
	      case Some( _ ) =>	next( rslt )
	      case nv @ _ => nv
	    }
	  }
	}	
      }
    }    
  }

  type KRsrc =
    Option[Resource] @scala.util.continuations.cpsParam[Option[Resource],Unit]

  // def assayK(
//     place : CnxnCtxtLabel[Namespace,Var,Tag]
//   ) : Option[Resource] @scala.util.continuations.cpsParam[Option[Resource],Unit]
//   = {
//     val contents = _labelMap.get( place )
//     contents match {
//       case sv @ Some( value ) => {
// 	shift { ( k : GetContinuation ) => { sv } }
//       }
//       case None => {
// 	val rslt = 
// 	  shift {
// 	    ( k : GetContinuation ) => {	      
// 	      _waiters( place ) =
// 		_waiters.get( place ).getOrElse( Nil ) ++ List( k )
// 	    }	    	      
// 	  }
// 	BasicLogService.reportage(
// 	  ( 
// 	    this
// 	    + "resuming with value : "
// 	    + rslt
// 	  )
// 	)
// 	_labelMap -= place
// 	rslt
//       }	
//     }
//   }

  // def fetch(
//     path : CnxnCtxtLabel[Namespace,Var,Tag]
//   ) : Seq[Option[Resource] @cpsParam[Option[Resource],Seq[Any]] ]
//   = {        
//     val hotspots = places( path, Input )    

//     def kLoop(
//       placeNSolns
//       : Seq[(CnxnCtxtLabel[Namespace,Var,Tag], Option[Solution[String]])],
//       acc
//       : Seq[Option[Resource] @cpsParam[Option[Resource],Unit]]
//     ) : Seq[Option[Resource] @cpsParam[Option[Resource],Unit]] = {
//       placeNSolns match {
// 	case placeNSoln :: rPlaceNSolns => {
// 	  val ( place, soln ) = placeNSoln
// 	  val assay : Seq[Option[Resource] @cpsParam[Option[Resource],Unit]]
// 	  = List( assayK( place ) )
// 	  kLoop( rPlaceNSolns, acc ++ assay )
// 	}
// 	case Nil => acc
//       }
//     }
    
//     kLoop( hotspots )
//   }

  override def put(
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    resource : Resource
  ) : Unit = {    
    for( placeNSoln <- places( path, Output ) ) {
      val ( place, soln ) = placeNSoln
      _waiters.get( place ) match {
	case Some( k :: ks ) => {
	  _waiters( place ) = ks
	  k( Some( RBound( Some( resource ), soln ) ) )
	}
	case Some( Nil ) => {
	  _labelMap( place ) = resource
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
