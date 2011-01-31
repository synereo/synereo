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
import scala.xml._
import scala.collection.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import org.prolog4j._

import org.exist.storage.DBBroker

import org.xmldb.api.base._
import org.xmldb.api.modules._
import org.xmldb.api._

import java.net.URI
import java.io.File
import java.io.FileInputStream

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

  trait PersistenceDescriptor {    
    self : CnxnXML[Namespace,Var,Tag]
	    with CnxnCtxtInjector[Namespace,Var,Tag] =>

    def db : Database
    def xmlCollStr : String
    def toFile( ptn : mTT.GetRequest ) : Option[File]
    def query( ptn : mTT.GetRequest ) : Option[String]
  }
  class ExistDescriptor(
    override val db : Database,
    override val xmlCollStr : String
  ) extends PersistenceDescriptor 
  with CnxnXML[Namespace,Var,Tag]
  with CnxnCtxtInjector[Namespace,Var,Tag] {
    override def query(
      ptn : mTT.GetRequest
    ) : Option[String] = {
      // TBD
      None
    }
    override def toFile(
      ptn : mTT.GetRequest
    ) : Option[File] = {
      // TBD
      None
    }
  }
  object ExistDescriptor {
    def apply(
      db : Database,
      xmlCollStr : String
    ) : ExistDescriptor = {
      new ExistDescriptor( db, xmlCollStr )
    }
    def unapply(
      ed : ExistDescriptor
    ) : Option[( Database, String )] = {
      Some( ( ed.db, ed.xmlCollStr ) )
    }
  }

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

    // def mget(
//       persist : Option[PersistenceDescriptor],
//       channels : Map[Place,Resource],
//       registered : Map[Place,List[RK]]
//     )( ptn : Pattern )
//     : Generator[Option[Resource],Unit,Unit] =
//       Generator {
// 	rk : ( Option[Resource] => Unit @suspendable ) =>
// 	  shift {
// 	    outerk : ( Unit => Unit ) =>
// 	      reset {
// 		val map =
// 		  Left[Map[Place,Resource],Map[Place,List[RK]]]( channels )
// 		val meets = locations( map, ptn )

// 		if ( meets.isEmpty )  {
// 		  val place = representative( ptn )
// 		  persist match {
// 		    case None => {
// 		      tweet(
// 			"did not find a resource, storing a continuation: " + rk
// 		      )
// 		      registered( place ) =
// 			registered.get( place ).getOrElse( Nil ) ++ List( rk )
// 		      rk( None )
// 		    }
// 		    case Some( pd ) => {
// 		      // TBD
// 		      for( qry <- pd.query( ptn ) ) {
// 			execute( qry )
// 		      }
// 		      rk( None )
// 		    }
// 		  }		  
// 		}
// 		else {
// 		  for(
// 		    placeNRrscNSubst <- itergen[PlaceInstance](
// 		      meets
// 		    )
// 		  ) {
// 		    val PlaceInstance( place, Left( rsrc ), s ) =
// 		      placeNRrscNSubst
		    
// 		    tweet( "found a resource: " + rsrc )		  
// 		    channels -= place
// 		    rk( s( rsrc ) )
		    
// 		    //shift { k : ( Unit => Unit ) => k() }
// 		  }
// 		}
// 		tweet( "get returning" )
// 		outerk()
// 	      }
// 	  }
//       }
   
//     def mput(
//       persist : Option[PersistenceDescriptor],
//       channels : Map[Place,Resource],
//       registered : Map[Place,List[RK]],
//       publish : Boolean      
//     )( ptn : Pattern, rsrc : Resource ) : Unit @suspendable = {    
//       for( placeNRKsNSubst <- putPlaces( channels, registered, ptn, rsrc ) ) {
// 	val PlaceInstance( wtr, Right( rks ), s ) = placeNRKsNSubst
// 	tweet( "waiters waiting for a value at " + wtr + " : " + rks )
// 	rks match {
// 	  case rk :: rrks => {	
// 	    if ( publish ) {
// 	      for( sk <- rks ) {
// 		spawn {
// 		  sk( s( rsrc ) )
// 		}
// 	      }
// 	    }
// 	    else {
// 	      registered( wtr ) = rrks
// 	      rk( s( rsrc ) )
// 	    }
// 	  }
// 	  case Nil => {
// 	    persist match {
// 	      case None => {
// 		channels( wtr ) = rsrc
// 	      }
// 	      case Some( pd ) => {
// 		store( pd.xmlCollStr )( rsrc )
// 	      }
// 	    }
// 	  }
// 	}
//       }
      
//     }
 
  }
}

object MonadicTS
 extends MonadicTermStoreScope[String,String,String,String] {
   type MTTypes = MonadicTermTypes[String,String,String,String]
   object TheMTT extends MTTypes
   override def protoTermTypes : MTTypes = TheMTT

   val aLabel =
     new CnxnCtxtLeaf[String,String,String](
       Left(
	 "a"
       )
     )
   val bLabel =
     new CnxnCtxtLeaf[String,String,String](
       Left(
	 "b"
       )
     )
   lazy val Mona = new MonadicTermStore()
 }

