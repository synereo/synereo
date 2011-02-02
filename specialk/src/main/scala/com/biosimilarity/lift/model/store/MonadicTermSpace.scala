// -*- mode: Scala;-*- 
// Filename:    MonadicTermSpace.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 31 01:46:38 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.util.continuations._ 
import scala.xml._
import scala.collection.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer

import org.prolog4j._

import org.exist.storage.DBBroker

import org.xmldb.api.base.{ Resource => XmlDbRrsc, _}
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
extends MonadicTermTypeScope[Namespace,Var,Tag,Value] 
  with MonadicDTSMsgScope[Namespace,Var,Tag,Value] {

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

  abstract class MonadicGeneratorJunction(
    override val name : URI,
    override val acquaintances : Seq[URI],
    override val requests : ListBuffer[Msgs.JTSReq],
    override val responses : ListBuffer[Msgs.JTSRsp],
    override val nameSpace :
    Option[LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]],
    override val traceMonitor : TraceMonitor[Msgs.DReq,Msgs.DRsp]
  )
  extends MonadicTermStore(
  ) with MonadicCollective
  with MonadicJSONAMQPDispatcher[Msgs.JTSReqOrRsp]
  with MonadicWireToTrgtConversion
  with MonadicGenerators {
    override type Wire = String
    override type Trgt = Msgs.JTSReqOrRsp

    override lazy val agentTwistedPairs
    : Map[URI,SemiMonadicAgentJSONAMQPTwistedPair[String]] =
      meetNGreet( acquaintances )

    def forwardGet( hops : List[URI], path : CnxnCtxtLabel[Namespace,Var,Tag] ) : Unit = {
      for(
	( uri, jsndr ) <- agentTwistedPairs
	if !hops.contains( uri )
      ) {
	reportage(
	  (
	    this
	    + " forwarding to "
	    + uri
	  )
	)
	val smajatp : SMAJATwistedPair =
	  jsndr.asInstanceOf[SMAJATwistedPair]
	
	smajatp.send(
	  Msgs.MDGetRequest[Namespace,Var,Tag,Value]( path ).asInstanceOf[Msgs.DReq]
	)
      }
    }
  }

  class InMemoryMonadicGeneratorJunction(
    override val name : URI,
    override val acquaintances : Seq[URI]
  ) extends MonadicGeneratorJunction(
    name,
    acquaintances,
    new ListBuffer[Msgs.JTSReq](),
    new ListBuffer[Msgs.JTSRsp](),
    Some( new LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]() ),
    AnAMQPTraceMonitor
  ) {
    def handleValue(
      dreq : Msgs.DReq,
      oV : Option[mTT.Resource],
      msrc : URI
    ) : Unit = {
      //tap( v )
      def sendRsp(
	atp : SemiMonadicAgentJSONAMQPTwistedPair[String],
	dreq : Msgs.DReq,	
	gv : Value
      ) = {
	val smajatp : SMAJATwistedPair =
	  atp.asInstanceOf[SMAJATwistedPair]
	smajatp.send(
	  dreq match {
	    case Msgs.MDGetRequest( path ) => {
	      Msgs.MDGetResponse[Namespace,Var,Tag,Value](
		path,
		gv
	      )
	    }
	    case Msgs.MDFetchRequest( path ) => {
	      Msgs.MDGetResponse[Namespace,Var,Tag,Value](
		path,
		gv
	      )
	    }
	  }
	)
      }      

      for(
	atp <- agentTwistedPairs.get( msrc );
	value <- oV
      ) {	

	value match {
	  case mTT.RBound(
	    Some( mTT.Ground( gv ) ),
	    Some( soln ) 
	  ) => {
	    reportage(
	      (
		this + " sending value " + oV + " back "
	      )
	    )
	    
	    sendRsp( atp, dreq, gv )
	      
	  }

	  case mTT.Ground( gv ) => {
	    reportage(
	      (
		this + " sending value " + oV + " back "
	      )
	    )
	    
	    sendRsp( atp, dreq, gv )

	  }
	  case _ => {
	    reportage(
	      (
		this 
		+ " not sending composite value " + oV
		+ " back "
	      )
	    )
	  }
	}
      }
      oV
    }

    def handleRequest( dreq : Msgs.JTSReq ) : Unit = {
      dreq match {
	case JustifiedRequest( 
	    msgId, mtrgt, msrc, lbl, body, _
	  ) => {
	    body match {
	      case dgreq@Msgs.MDGetRequest( path ) => {
		reportage( this + "handling : " + dgreq	)
		
		reportage(
		  ( this + "getting locally for location : " + path )
		)
		reset {
		  for( v <- get( List( msrc ) )( path ) ) {
		    reportage(
		      (
			this 
			+ " returning from local get for location : "
			+ path
			+ "\nwith value : " + v
		      )
		    )
		    handleValue( dgreq, v, msrc )
		  }
		}
	      }

	      case dfreq@Msgs.MDFetchRequest( path ) => {
		reportage( this + "handling : " + dfreq	)
		
		reportage(
		  ( this + "fetching locally for location : " + path )
		)
		reset {
		  for( v <- fetch( List( msrc ) )( path ) ) {
		    reportage(
		      (
			this 
			+ " returning from local fetch for location : "
			+ path
			+ "\nwith value : " + v
		      )
		    )
		    handleValue( dfreq, v, msrc )
		  }
		}
	      }

	      case dpreq@Msgs.MDPutRequest( path, value ) => {	
		reportage( this + "handling : " + dpreq	)		
		reset { put( path, mTT.Ground( value ) ) }
	      }
	    }
	  }
      }
    }

    def handleResponse( drsp : Msgs.JTSRsp ) : Unit = {
      drsp match {
	case JustifiedResponse( 
	  msgId, mtrgt, msrc, lbl, body, _
	) => {
	  body match {
	    case Msgs.MDGetResponse( path, value ) => {
	      reset { put( path, mTT.Ground( value ) ) }
	    }
	    case Msgs.MDFetchResponse( path, value ) => {
	      reset { put( path, mTT.Ground( value ) ) }
	    }
	    case dput : Msgs.MDPutResponse[Namespace,Var,Tag,Value] => {	
	    }
	    case _ => {
	      reportage(
		(
		  this 
		  + " handling unexpected message : " + body
		)
	      )
	    }
	  }
	}
      }
    }

    def handleIncoming( dmsg : Msgs.JTSReqOrRsp ) : Unit = {
      dmsg match {
	case Left(
	  dreq@JustifiedRequest( 
	    msgId, mtrgt, msrc, lbl, body, _
	  )
	) => {
	  reportage(
	    (
	      this + " handling : " + dmsg
	      + " from " + msrc
	      + " on behalf of " + mtrgt
	    )
	  )
	  handleRequest( dreq )
	}
	case Right(
	  drsp@JustifiedResponse( 
	    msgId, mtrgt, msrc, lbl, body, _
	  )
	) => {
	  reportage(
	    (
	      this + " handling : " + dmsg
	      + " from " + msrc
	      + " on behalf of " + mtrgt
	    )
	  )
	  handleResponse( drsp )
	}
      }
    }

    def mget( hops : List[URI] )(
      channels :
      Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : Boolean
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      Generator {
	rk : ( Option[mTT.Resource] => Unit @suspendable ) =>
	  shift {
	    outerk : ( Unit => Unit ) =>
	      reset {
		for(
		  oV <- mget( channels, registered, consume )( path ) 
		) {
		  oV match {
		    case None => {
		      forwardGet( hops, path )
		      rk( oV )
		    }
		    case _ => rk( oV )
		  }
		}
	      }
	  }
      }
    }
  
    def get( hops : List[URI] )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      mget( Nil )( theMeetingPlace, theWaiters, true )( path )    
    }

    override def get(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      get( Nil )( path )    
    }

    def fetch( hops : List[URI] )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      mget( Nil )( theMeetingPlace, theWaiters, false )( path )    
    }

    override def fetch(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      fetch( Nil )( path )    
    }
  }
}

object MonadicTS
 extends MonadicTermStoreScope[String,String,String,String] 
  with UUIDOps {
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

   val cLabel =
    new CnxnCtxtLeaf[String,String,String](
      Left(
	"c"
      )
    )
  val dLabel =
    new CnxnCtxtLeaf[String,String,String](
      Left(
	"d"
      )
    )

   lazy val Mona = new MonadicTermStore()

   type MsgTypes = DTSMSH[String,String,String,String]   

   val protoDreqUUID = getUUID()
   val protoDrspUUID = getUUID()    
  
   object MonadicDMsgs extends MsgTypes {
     
     override def protoDreq : DReq = MDGetRequest( aLabel )
     override def protoDrsp : DRsp = MDGetResponse( aLabel, aLabel.toString )
     override def protoJtsreq : JTSReq =
       JustifiedRequest(
	 protoDreqUUID,
	 new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	 new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	 getUUID(),
	 protoDreq,
	 None
       )
     override def protoJtsrsp : JTSRsp = 
       JustifiedResponse(
	 protoDreqUUID,
	 new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
	 new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
	 getUUID(),
	 protoDrsp,
	 None
       )
     override def protoJtsreqorrsp : JTSReqOrRsp =
       Left( protoJtsreq )
   }
   
   override def protoMsgs : MsgTypes = MonadicDMsgs
 }

