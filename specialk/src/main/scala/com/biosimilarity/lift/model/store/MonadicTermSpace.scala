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

import org.exist.util.serializer.SAXSerializer
import org.exist.util.serializer.SerializerPool

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import javax.xml.transform.OutputKeys
import java.util.Properties
import java.net.URI
import java.io.File
import java.io.FileInputStream
import java.io.OutputStreamWriter

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

    case class AgentCnxn(
      override val src : URI,
      override val label : String,
      override val trgt : URI
    ) extends Cnxn[URI,String,URI]
  
  trait PersistenceDescriptor {    
    self : CnxnXML[Namespace,Var,Tag]
	    with CnxnCtxtInjector[Namespace,Var,Tag] =>

    def db : Database
    def xmlCollStr( cnxn : AgentCnxn ) : String
    def queryServiceType : String = "XQueryService"
    def queryServiceVersion : String = "1.0"
    def toFile( ptn : mTT.GetRequest ) : Option[File]
    def query( ptn : mTT.GetRequest ) : Option[String]

    def kvNameSpace : Namespace
    def asValue(
      rsrc : mTT.Resource
    ) : CnxnCtxtLeaf[Namespace,Var,Tag]
    def asKey(
      key : mTT.GetRequest
    ) : mTT.GetRequest with Factual = {
      key match {
	case leaf : CnxnCtxtLeaf[Namespace,Var,Tag] =>
	  leaf
	case branch : CnxnCtxtBranch[Namespace,Var,Tag] =>
	  branch
      }
    }

    def asRecord(
      key : mTT.GetRequest,
      value : mTT.Resource
    ) : mTT.GetRequest with Factual = {
      new CnxnCtxtBranch[Namespace,Var,Tag](
	kvNameSpace,
	List( asKey( key ), asValue( value ) )
      )
    }
  }

  abstract class ExistDescriptor(
    override val db : Database
  ) extends PersistenceDescriptor 
    with CnxnXQuery[Namespace,Var,Tag]
  with CnxnXML[Namespace,Var,Tag]
  with CnxnCtxtInjector[Namespace,Var,Tag]
  with UUIDOps {
    override def xmlCollStr( cnxn : AgentCnxn ) : String = {
      // TBD
      cnxn.src.getHost + cnxn.trgt.getHost
    }
    override def query(
      ptn : mTT.GetRequest
    ) : Option[String] = {
      // TBD
      Some( xqQuery( ptn ) )
    }
    override def toFile(
      ptn : mTT.GetRequest
    ) : Option[File] = {
      // TBD
      None
    }
  }
  object ExistDescriptor {
    // def apply(
//       db : Database,
//       xmlCollStr : String
//     ) : ExistDescriptor = {
//       new ExistDescriptor( db )
//     }
    def unapply(
      ed : ExistDescriptor
    ) : Option[( Database )] = {
      Some( ( ed.db ) )
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
  with ConfiggyReporting
  with ConfiggyJournal
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

    // BUGBUG: LGM -- refactor this!
    override def tmpDirStr : String = {
      val tds = config.getString( "storageDir", "tmp" )       
      val tmpDir = new java.io.File( tds )
      if ( ! tmpDir.exists ) {
	tmpDir.mkdir
      }
      tds
    }

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
    override def toString() : String = {
      name + " -> " + acquaintances
    }

    override type Wire = String
    override type Trgt = Msgs.JTSReqOrRsp

    override lazy val agentTwistedPairs
    : Map[URI,SemiMonadicAgentJSONAMQPTwistedPair[String]] =
      meetNGreet( acquaintances )

    trait Ask
    case object AGet extends Ask
    case object AFetch extends Ask
    case object ASubscribe extends Ask

    def forward(
      ask : Ask,
      hops : List[URI],
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Unit = {

      reportage(
	( this + " in forwardGet with hops: " + hops )
      )
      
      for(
	( uri, jsndr ) <- agentTwistedPairs
	if !hops.contains( uri )
      ) {
	reportage(
	  ( this + " forwarding to " + uri )
	)
	val smajatp : SMAJATwistedPair =
	  jsndr.asInstanceOf[SMAJATwistedPair]
	
	smajatp.send(
	  ask match {
	    case AGet => {
	      Msgs.MDGetRequest[Namespace,Var,Tag,Value](
		path
	      ).asInstanceOf[Msgs.DReq]
	    }
	    case AFetch => {
	      Msgs.MDFetchRequest[Namespace,Var,Tag,Value](
		path
	      ).asInstanceOf[Msgs.DReq]
	    }
	    case ASubscribe => {
	      Msgs.MDSubscribeRequest[Namespace,Var,Tag,Value](
		path
	      ).asInstanceOf[Msgs.DReq]
	    }
	  }
	)
      }
    }
  }

  class DistributedMonadicGeneratorJunction(
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
    def sendRsp(
      atp : SemiMonadicAgentJSONAMQPTwistedPair[String],
      dreq : Msgs.DReq,	
      oGv : Option[Value]
    ) = {
      val smajatp : SMAJATwistedPair =
	atp.asInstanceOf[SMAJATwistedPair]
      
      smajatp.send(
	dreq match {
	  case Msgs.MDGetRequest( path ) => {
	    oGv match {
	      case Some( gv ) => {
		Msgs.MDGetResponse[Namespace,Var,Tag,Value](
		  path,
		  gv
		)
	      }
	      case None => {
		throw new Exception( "get must take value" )
	      }
	    }
	  }
	  case Msgs.MDFetchRequest( path ) => {
	    oGv match {
	      case Some( gv ) => {
		Msgs.MDFetchResponse[Namespace,Var,Tag,Value](
		  path,
		  gv
		)
	      }
	      case None => {
		throw new Exception( "fetch must take value" )
	      }
	    }
	  }
	  case Msgs.MDSubscribeRequest( path ) => {
	    oGv match {
	      case Some( gv ) => {
		Msgs.MDSubscribeResponse[Namespace,Var,Tag,Value](
		  path,
		  gv
		)
	      }
	      case None => {
		throw new Exception( "subscribe must take value" )
	      }
	    }
	  }
	  case Msgs.MDPutRequest( path, _ ) => {
	    Msgs.MDPutResponse[Namespace,Var,Tag,Value](
	      path
	    )
	  }
	  case Msgs.MDPublishRequest( path, _ ) => {
	    Msgs.MDPublishResponse[Namespace,Var,Tag,Value](
	      path
	    )
	  }
	}
      )
    }

    def handleValue(
      dreq : Msgs.DReq,
      oV : Option[mTT.Resource],
      msrc : URI
    ) : Unit = {
      //tap( v )            

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
	    
	    sendRsp( atp, dreq, Some( gv ) )
	      
	  }

	  case mTT.Ground( gv ) => {
	    reportage(
	      (
		this + " sending value " + oV + " back "
	      )
	    )
	    
	    sendRsp( atp, dreq, Some( gv ) )

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
      val JustifiedRequest( 
	msgId, mtrgt, msrc, lbl, body, _
      ) = dreq

      reportage( this + "handling : " + dreq	)

      body match {
	case dgreq@Msgs.MDGetRequest( path ) => {	  
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

	case dsreq@Msgs.MDSubscribeRequest( path ) => {
	  reportage(
	    ( this + "fetching locally for location : " + path )
	  )
	  reset {
	    for( v <- subscribe( List( msrc ) )( path ) ) {
	      reportage(
		(
		  this 
		  + " returning from local subscribe for location : "
		  + path
		  + "\nwith value : " + v
		)
	      )
	      handleValue( dsreq, v, msrc )
	    }
	  }
	}
	
	case dpreq@Msgs.MDPutRequest( path, value ) => {	
	  reset { put( path, mTT.Ground( value ) ) }
	  for( atp <- agentTwistedPairs.get( msrc ) ) {
	    sendRsp( atp, dpreq, None )
	  }
	}
	case dpbreq@Msgs.MDPublishRequest( path, value ) => {	
	  reset { publish( path, mTT.Ground( value ) ) }
	  for( atp <- agentTwistedPairs.get( msrc ) ) {
	    sendRsp( atp, dpbreq, None )
	  }
	}
      }
    }
    
    def handleResponse( drsp : Msgs.JTSRsp ) : Unit = {      
      val JustifiedResponse( 
	  msgId, mtrgt, msrc, lbl, body, _
      ) = drsp

      body match {
	case Msgs.MDGetResponse( path, value ) => {
	  reset { put( path, mTT.Ground( value ) ) }
	}
	case Msgs.MDFetchResponse( path, value ) => {
	  reset { put( path, mTT.Ground( value ) ) }
	}
	case Msgs.MDSubscribeResponse( path, value ) => {
	  reset { publish( path, mTT.Ground( value ) ) }
	}
	case dput : Msgs.MDPutResponse[Namespace,Var,Tag,Value] => {	
	}
	case dpub : Msgs.MDPublishResponse[Namespace,Var,Tag,Value] => {	
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

    def mget( ask : Ask, hops : List[URI] )(
      channels : Map[mTT.GetRequest,mTT.Resource],
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
		      forward( ask, hops, path )
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
      mget( AGet, hops )( theMeetingPlace, theWaiters, true )( path )    
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
      mget( AFetch, hops )( theMeetingPlace, theWaiters, false )( path )    
    }

    override def fetch(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      fetch( Nil )( path )    
    }

    def subscribe( hops : List[URI] )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      mget( ASubscribe, hops )( theChannels, theSubscriptions, true )( path )    
    }

    override def subscribe(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      subscribe( Nil )( path )    
    }
  }

  trait KVTrampoline {
    def persistenceDescriptor : Option[PersistenceDescriptor]

    def kvNameSpace : Option[Namespace] = {
      for( pd <- persistenceDescriptor )
	yield { pd.kvNameSpace }
    }
    def asValue(
      rsrc : mTT.Resource
    ) : Option[CnxnCtxtLeaf[Namespace,Var,Tag]] = {
      for( pd <- persistenceDescriptor ) 
	yield { pd.asValue( rsrc ) }
    }
    def asKey(
      key : mTT.GetRequest
    ) : Option[mTT.GetRequest with Factual] = {
      for( pd <- persistenceDescriptor )
	yield { pd.asKey( key ) }
    }

    def asRecord(
      key : mTT.GetRequest,
      value : mTT.Resource
    ) : Option[mTT.GetRequest with Factual] = {
      for( pd <- persistenceDescriptor )
	yield { pd.asRecord( key, value ) }
    }    
  }

  abstract class PersistedtedMonadicGeneratorJunction(
    override val name : URI,
    override val acquaintances : Seq[URI]
  ) extends DistributedMonadicGeneratorJunction(
    name,
    acquaintances
  ) with KVTrampoline {    
    def putInStore(
      persist : Option[PersistenceDescriptor],
      channels : Map[mTT.GetRequest,mTT.Resource],
      ptn : mTT.GetRequest,
      wtr : Option[mTT.GetRequest],
      rsrc : mTT.Resource
    )( cnxn : AgentCnxn ) : Unit = {
      persist match {
	case None => {
	  channels( wtr.getOrElse( ptn ) ) = rsrc	  
	}
	case Some( pd ) => {
	  tweet( "accessing db : " + pd.db )
	  // remove this line to force to db on get
	  channels( wtr.getOrElse( ptn ) ) = rsrc	  
	  spawn {
	    val rcrd = pd.asRecord( ptn, rsrc )
	    tweet(
	      (
		"storing to db : " + pd.db
		+ " pair : " + rcrd
		+ " in coll : " + pd.xmlCollStr( cnxn )
	      )
	    )
	    store( pd.xmlCollStr( cnxn ) )( rcrd )
	  }
	}
      }
    }

     def putPlaces( persist : Option[PersistenceDescriptor] )(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      ptn : mTT.GetRequest,
      rsrc : mTT.Resource
    )( cnxn : AgentCnxn ) : Generator[PlaceInstance,Unit,Unit] = {    
      Generator {
	k : ( PlaceInstance => Unit @suspendable ) => 
	  // Are there outstanding waiters at this pattern?    
	  val map =
	    Right[
	      Map[mTT.GetRequest,mTT.Resource],
	      Map[mTT.GetRequest,List[RK]]
	    ]( registered )
	  val waitlist = locations( map, ptn )

	  waitlist match {
	    // Yes!
	    case waiter :: waiters => {
	      tweet( "found waiters waiting for a value at " + ptn )
	      val itr = waitlist.toList.iterator	    
	      while( itr.hasNext ) {
		k( itr.next )
	      }
	    }
	    // No...
	    case Nil => {
	      // Store the rsrc at a representative of the ptn
	      tweet( "no waiters waiting for a value at " + ptn )
	      //channels( representative( ptn ) ) = rsrc
	      putInStore(
		persist, channels, ptn, None, rsrc 
	      )(
		cnxn
	      )
	    }
	  }
      }
    }
    
    def mput( persist : Option[PersistenceDescriptor] )(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : Boolean
    )( cnxn : AgentCnxn )(
      ptn : mTT.GetRequest,
      rsrc : mTT.Resource
    ) : Unit @suspendable = {    
      for(
	placeNRKsNSubst
	<- putPlaces(
	  persist
	)( channels, registered, ptn, rsrc )( cnxn )
      ) {
	val PlaceInstance( wtr, Right( rks ), s ) = placeNRKsNSubst
	tweet( "waiters waiting for a value at " + wtr + " : " + rks )
	rks match {
	  case rk :: rrks => {	
	    if ( consume ) {
	      for( sk <- rks ) {
		spawn {
		  sk( s( rsrc ) )
		}
	      }
	    }
	    else {
	      registered( wtr ) = rrks
	      rk( s( rsrc ) )
	    }
	  }
	  case Nil => {
	    putInStore(
	      persist, channels, ptn, Some( wtr ), rsrc 
	    )( cnxn )
	  }
	}
      }
      
    }
    def mget(
      persist : Option[PersistenceDescriptor],
      ask : Ask,
      hops : List[URI]
    )(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : Boolean
    )( cnxn : AgentCnxn )(
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
		      persist match {
			case None => {
			  forward( ask, hops, path )
			  rk( oV )
			}
			case Some( pd ) => {
			  tweet(
			    "accessing db : " + pd.db
			  )
			  val query = pd.query( path )
			  query match {
			    case None => {
			      forward( ask, hops, path )
			    }
			    case Some( qry ) => {
			      spawn {
				for(			    
				  xmlColl <- getCollection(
				    true
				  )( pd.xmlCollStr( cnxn ) )
				) {
				  val srvc : Service =
				    getQueryService( xmlColl )(
				      pd.queryServiceType,
				      pd.queryServiceVersion
				    );
				  tweet(
				    (
				      "querying db : " + pd.db
				      + " from coll " + pd.xmlCollStr( cnxn )
				      + " where " + qry
				    )
				  )
				  val rsrcSet =
				    execute( xmlColl )( srvc )( qry )
				  if ( rsrcSet.getSize == 0 ) {
				    forward( ask, hops, path )
				  }
				  else {
				    // TBD
				    // Need to put results
				  }
				}
			      }
			    }
			    rk( oV )
			  }
			}		      
		      }
		    }
		    case _ => rk( oV )
		  }
		}
	      }
	  }
      }
    }
    
    def put( cnxn : AgentCnxn )(
      ptn : mTT.GetRequest, rsrc : mTT.Resource
    ) =
      mput( persistenceDescriptor )(
	theMeetingPlace, theWaiters, false
      )( cnxn )( ptn, rsrc )
    def publish( cnxn : AgentCnxn )(
      ptn : mTT.GetRequest, rsrc : mTT.Resource
    ) =
      mput( persistenceDescriptor )(
	theChannels, theSubscriptions, true
      )( cnxn )( ptn, rsrc )

    def get( hops : List[URI] )(
      cnxn : AgentCnxn
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      mget( persistenceDescriptor, AGet, hops )(
	theMeetingPlace, theWaiters, true
      )( cnxn )( path )    
    }
    def get(
      cnxn : AgentCnxn
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      get( Nil )( cnxn )( path )    
    }

    def fetch( hops : List[URI] )(
      cnxn : AgentCnxn
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      mget( persistenceDescriptor, AFetch, hops )(
	theMeetingPlace, theWaiters, false
      )( cnxn )( path )    
    }
    def fetch(
      cnxn : AgentCnxn
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      fetch( Nil )( cnxn )( path )    
    }

    def subscribe( hops : List[URI] )(
      cnxn : AgentCnxn
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      mget( persistenceDescriptor, ASubscribe, hops )(
	theChannels, theSubscriptions, true
      )( cnxn )( path )    
    }
    def subscribe(
      cnxn : AgentCnxn
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      subscribe( Nil )( cnxn )( path )    
    }
  }
}

object MonadicTS
 extends MonadicTermStoreScope[String,String,String,String] 
  with UUIDOps {
    import AgentURIDefaults._
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
    def Imma( a : String, b : String )  =
      new DistributedMonadicGeneratorJunction( a, List( b ) )
    
    class PersistedtedStringMGJ(
      override val name : URI,
      override val acquaintances : Seq[URI]
    ) extends PersistedtedMonadicGeneratorJunction(
      name, acquaintances
    ) {
      class StringExistDescriptor
      extends ExistDescriptor( database ) {
	def kvNameSpace : String = "KVPairs"
	def asValue(
	  rsrc : mTT.Resource
	) : CnxnCtxtLeaf[String,String,String] = {
	  val blob =
	    new XStream( new JettisonMappedXmlDriver ).toXML( rsrc )
	  //asXML( rsrc )
	  new CnxnCtxtLeaf[String,String,String](
	    Left[String,String]( blob )
	  )
	}
      
      }

      def persistenceDescriptor : Option[PersistenceDescriptor] =
	Some(
	  new StringExistDescriptor
	)
    }
    
    def Pimma( a : String, b : String )  =
      new PersistedtedStringMGJ( a, List( b ) )

    import scala.collection.immutable.IndexedSeq

    def testQuery( pimmgJunq : PersistedtedStringMGJ )(
      cnxn : AgentCnxn
    )(
      qryStr : String
    ) : Option[IndexedSeq[org.xmldb.api.base.Resource]] = {
      for(
	pd <- pimmgJunq.persistenceDescriptor;
	xmlColl <- pimmgJunq.getCollection( true )( pd.xmlCollStr( cnxn ) )
      )
      yield {
	val xqSrvc =
	  pimmgJunq.getQueryService( xmlColl )(
	    "XQueryService", "1.0"
	  ).asInstanceOf[XQueryService]

	val rsrcSet = xqSrvc.execute( xqSrvc.compile( qryStr ) )
	println( "number of results = " + rsrcSet.getSize )

	val outputProperties : Properties = new Properties()
        outputProperties.setProperty(
	  OutputKeys.INDENT, "yes"
	)
	
	val serializer : SAXSerializer =	  
	  SerializerPool.getInstance().borrowObject(
            Class.forName( "org.exist.util.serializer.SAXSerializer" )
	  ).asInstanceOf[SAXSerializer]	

	val rslt = 
	  for( i <- 0 to rsrcSet.getSize.toInt - 1 )
	  yield {
	    val rsrc = rsrcSet.getResource( i )
	    val xmlRsrc = rsrc.asInstanceOf[XMLResource]

	    val bufStrm = new java.io.ByteArrayOutputStream()

	    serializer.setOutput(
	      new OutputStreamWriter(bufStrm),
	      outputProperties
	    )	    

	    xmlRsrc.getContentAsSAX( serializer )
	    
	    println( "the resource is : " + bufStrm )

	    rsrc
	  }

	SerializerPool.getInstance().returnObject(serializer);
	
	rslt
      }
    }
    
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

