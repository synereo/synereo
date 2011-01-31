// -*- mode: Scala;-*- 
// Filename:    MonadicTermSpace.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 31 01:46:38 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.lib._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.util.continuations._ 
import scala.collection.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import org.prolog4j._

import java.net.URI

trait MonadicTermTypes[Namespace,Var,Tag,Value] {
  trait Resource
  case class Ground( v : Value ) extends Resource
  case class RMap(
    m : TMapR[Namespace,Var,Tag,Value]
  ) extends Resource
  case class RBound(
    rsrc : Option[Resource], soln : Option[Solution[String]]
  ) extends Resource

  type GetRequest = CnxnCtxtLabel[Namespace,Var,Tag]  

  class TMapR[Namespace,Var,Tag,Value]
  extends HashMap[GetRequest,Resource]  
}

trait MonadicTermTypeScope[Namespace,Var,Tag,Value] {
  type MTTypes <: MonadicTermTypes[Namespace,Var,Tag,Value]
  def protoTermTypes : MTTypes
  val mTT : MTTypes = protoTermTypes
}

trait MonadicTermStoreScope[Namespace,Var,Tag,Value] 
extends MonadicTermTypeScope[Namespace,Var,Tag,Value] {
  class MonadicTermStore(
  )
  extends MonadicTupleSpace[mTT.GetRequest,mTT.GetRequest,mTT.Resource] 
  with CnxnStorage[Namespace,Var,Tag]
  with CnxnCtxtInjector[Namespace,Var,Tag]
  with CnxnUnificationCompositeTermQuery[Namespace,Var,Tag]
  with CnxnConversions[Namespace,Var,Tag]
  with CnxnXML[Namespace,Var,Tag]
  with XMLStore
  with WireTap
  with Journalist
  with UUIDOps {
    override def tap [A] ( fact : A ) : Unit = {
      reportage( fact )
    }
    
    override def URI : String = ExistDefaults.URI
    override def driver : String = ExistDefaults.driver
    override def dbRoot : String = ExistDefaults.dbRoot
    override def createDB : Boolean = ExistDefaults.createDB
    override def indent : Boolean = ExistDefaults.indent
    override def resourceType : String = ExistDefaults.resourceType  
    
    override def tmpDirStr : String = CnxnStorageDefaults.tmpDirStr

    override val theMeetingPlace =
      new mTT.TMapR[Namespace,Var,Tag,Value]()
    override val theChannels =
      new mTT.TMapR[Namespace,Var,Tag,Value]()
    override val theWaiters =
      new TMapK[Namespace,Var,Tag,Value]()
    override val theSubscriptions =
      new TMapK[Namespace,Var,Tag,Value]()

    class TMapK[Namespace,Var,Tag,Value]
    extends HashMap[mTT.GetRequest,List[RK]]

    case class PrologSubstitution( soln : Solution[String] )
	 extends Function1[mTT.Resource,Option[mTT.Resource]] {
	   override def apply( rsrc : mTT.Resource ) = {
	     Some( mTT.RBound( Some( rsrc ), Some( soln ) ) )
	   }
	 }

    override type Substitution = PrologSubstitution
    
    override def representative(
      ptn : mTT.GetRequest
    ) : mTT.GetRequest = {
      ptn
    }
    override def fits(
      ptn : mTT.GetRequest,
      place : mTT.GetRequest
    ) : Boolean = {
      matches( ptn, ptn ) match {
	case Some( soln ) => {
	  //PrologSubstitution( soln )
	  true
	}
	case None => {
	  false
	}
      }
    }

    override def fitsK(
      ptn : mTT.GetRequest,
      place : mTT.GetRequest
    ) : Option[Substitution] = {
      matches( ptn, ptn ) match {
	case Some( soln : Solution[String] ) => {
	  Some( PrologSubstitution( soln ) )
	}
	case None => {
	  None
	}
      }
    }
    
  }
}

object MonadicTS
 extends MonadicTermStoreScope[String,String,String,String] {
   type MTTypes = MonadicTermTypes[String,String,String,String]
   object TheMTT extends MTTypes
   override def protoTermTypes : MTTypes = TheMTT
 }

