// -*- mode: Scala;-*- 
// Filename:    MonadicJunction.scala 
// Authors:     lgm                                                    
// Creation:    Fri Jan 21 20:36:16 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import net.liftweb.amqp._

import scala.util.continuations._ 
import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.actors.Actor
import scala.actors.Actor._

import com.rabbitmq.client._

import org.prolog4j._

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.util.UUID
import java.net.URI
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream

trait MonadicDTSMsgScope[Namespace,Var,Tag,Value]
extends DTSMsgScope[Namespace,Var,Tag,Value]
{
  import AMQPDefaults._

  object AnAMQPTraceMonitor extends TraceMonitor[Msgs.DReq,Msgs.DRsp]  

  class StdMonadicAgentJSONAMQPDispatcher[TxPort](
    host : String, port : Int,
    override val requests : ListBuffer[Msgs.JTSReq],
    override val responses : ListBuffer[Msgs.JTSRsp],
    override val nameSpace :
    Option[LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]],
    override val traceMonitor : TraceMonitor[Msgs.DReq,Msgs.DRsp]
  ) extends StdMonadicJSONAMQPDispatcher[Msgs.JTSReqOrRsp](
    host, port
  ) with MonadicAgency[String,Msgs.DReq,Msgs.DRsp] {  
    override type Wire = String
    override type Trgt = Msgs.JTSReqOrRsp
    override def name : URI = {
      new URI( "agent", host, "/", "" )
    }    
  }

  trait SemiMonadicAgentJSONAMQPTwistedPair[TxPort]
  extends MonadicAgency[TxPort,Msgs.DReq,Msgs.DRsp]
  with SemiMonadicJSONAMQPTwistedPair[Msgs.JTSReqOrRsp] {
    self : MonadicWireToTrgtConversion with MonadicGenerators with WireTap with Journalist =>
      
    override type Trgt = Msgs.JTSReqOrRsp
    override def jsonDispatcher( handle : Msgs.JTSReqOrRsp => Unit )(
      implicit dispatchOnCreate : Boolean, port : Int
    ) : StdMonadicJSONAMQPDispatcher[Msgs.JTSReqOrRsp] = {
      _jsonDispatcher match {
	case Some( jd ) => jd
	case None => {
	  val jd =
	    new StdMonadicAgentJSONAMQPDispatcher[Msgs.JTSReqOrRsp](
	      srcURI.getHost, port,
	      new ListBuffer[Msgs.JTSReq](),
	      new ListBuffer[Msgs.JTSRsp](),
	      Some( new LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]() ),
	      AnAMQPTraceMonitor
	    )
	  
	  if ( dispatchOnCreate ) {
	    reset {
	      for( msg <- jd.xformAndDispatch( jd.beginService() ) ){
		handle( msg )
	      }
	    }	
	  }
	  
	  _jsonDispatcher = Some( jd )
	  jd
	}
      }
    }
  }

  class SMAJATwistedPair(
    override val srcURI : URI,
    override val trgtURI : URI
  ) extends SemiMonadicAgentJSONAMQPTwistedPair[String] 
  with MonadicJSONAMQPDispatcher[Msgs.JTSReqOrRsp]
  with MonadicWireToTrgtConversion with MonadicGenerators with WireTap
  with Journalist
  with ConfiggyReporting
  with ConfiggyJournal
  with UUIDOps {
      override type Wire = String
      override type Trgt = Msgs.JTSReqOrRsp
      override def tap [A] ( fact : A ) : Unit = {
	reportage( fact )
      }
      def jsonDispatcher() : StdMonadicAgentJSONAMQPDispatcher[Msgs.JTSReqOrRsp] = {
	jsonDispatcher( ( x ) => {} ).asInstanceOf[StdMonadicAgentJSONAMQPDispatcher[Msgs.JTSReqOrRsp]]
      }
      override def name : URI = srcURI
      override def requests : ListBuffer[Msgs.JTSReq] = {
	jsonDispatcher().requests
      }
      override def responses : ListBuffer[Msgs.JTSRsp] = {
	jsonDispatcher( ).responses
      }
      override def nameSpace : Option[LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]] = {
	jsonDispatcher( ).nameSpace
    }
      override def traceMonitor : TraceMonitor[Msgs.DReq,Msgs.DRsp] =
    {
      jsonDispatcher( ).traceMonitor
    }

    def send( dreq : Msgs.DReq ) : Unit = {
      tweet(
	(
	  this
	  + " is sending : "
	  + dreq
	  + " on behalf of "
	  + srcURI
	  + " to "
	  + trgtURI
	)
      )
      val jr = JustifiedRequest[Msgs.DReq,Msgs.DRsp](
	getUUID(),
	trgtURI,
	srcURI,
	getUUID(),
	dreq,
	None
      )
      
      send( Left[Msgs.JTSReq,Msgs.JTSRsp]( jr ) )
    }

    def send( drsp : Msgs.DRsp ) : Unit = {
      tweet(
	(
	  this
	  + " is sending : "
	  + drsp
	  + " on behalf of "
	  + srcURI
	  + " to "
	  + trgtURI
	)
      )
      val jr = JustifiedResponse[Msgs.DReq,Msgs.DRsp](
	getUUID(),
	trgtURI,
	srcURI,
	getUUID(),
	drsp,
	None
      )
      
      send( Right[Msgs.JTSReq,Msgs.JTSRsp]( jr ) )
    }
  }

  object SMAJATwistedPair {
    def apply (
      srcIPStr : String, trgtIPStr : String
    ) : SMAJATwistedPair = {
      new SMAJATwistedPair(
	new URI( "agent", srcIPStr, "/", "" ),
	new URI( "agent", trgtIPStr, "/", "" )
      )
    }
    def unapply(
      smajatp : SMAJATwistedPair
    ) : Option[(URI,URI)] = {
      Some( ( smajatp.srcURI, smajatp.trgtURI ) )
    }    
  }

  trait MonadicCollective 
  extends MonadicAgency[String,Msgs.DReq,Msgs.DRsp] {
    self : MonadicWireToTrgtConversion
	with MonadicGenerators with WireTap with Journalist =>
    
    def agentTwistedPairs :
    Map[URI,SemiMonadicAgentJSONAMQPTwistedPair[String]]
    def acquaintances : Seq[URI]

    def handleIncoming( dmsg : Msgs.JTSReqOrRsp ) : Unit
    
    def meetNGreet( acquaintances : Seq[URI] )
    : Map[URI,SemiMonadicAgentJSONAMQPTwistedPair[String]] =
      {
	val map = new HashMap[URI,SemiMonadicAgentJSONAMQPTwistedPair[String]]()
	for( acquaintance <- acquaintances )
	yield {
	  val atp =
	    new SMAJATwistedPair(
	      name,
	      acquaintance
	    )      
	  
	  atp.jsonSender // activate jsonSender
	  atp.jsonDispatcher( handleIncoming ) // activate jsonDispatcher
	  map( acquaintance ) = atp	

	}
	map
      }
  }

  abstract class MonadicJunction(
    override val name : URI,
    override val acquaintances : Seq[URI],
    override val requests : ListBuffer[Msgs.JTSReq],
    override val responses : ListBuffer[Msgs.JTSRsp],
    override val nameSpace :
      Option[LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]],
    override val traceMonitor : TraceMonitor[Msgs.DReq,Msgs.DRsp]
  )
  extends TermStore[Namespace,Var,Tag,Value](
  ) with MonadicCollective
  with MonadicJSONAMQPDispatcher[Msgs.JTSReqOrRsp]
  with MonadicWireToTrgtConversion
  with MonadicGenerators
  with WireTap
  with Journalist
  with ConfiggyReporting
  with ConfiggyJournal {
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
	tweet(
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

  class InMemoryMonadicJunction(
    override val name : URI,
    override val acquaintances : Seq[URI]
  ) extends MonadicJunction(
    name,
    acquaintances,
    new ListBuffer[Msgs.JTSReq](),
    new ListBuffer[Msgs.JTSRsp](),
    Some( new LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]() ),
    AnAMQPTraceMonitor
  ) {
    def handleRequest( dreq : Msgs.JTSReq ) : Unit = {
      dreq match {
	case JustifiedRequest( 
	    msgId, mtrgt, msrc, lbl, body, _
	  ) => {
	    body match {
	      case dgreq@Msgs.MDGetRequest( path ) => {
		tweet(
		  (
		    this 
		    + "handling : "
		    + dgreq
		  )
		)

		val k =
		  {
		    ( v : Option[Resource] ) => {
		      //tap( v )
		      for(
			atp <- agentTwistedPairs.get( msrc );
			value <- v
		      ) {
			val smajatp : SMAJATwistedPair =
			  atp.asInstanceOf[SMAJATwistedPair]

			tweet(
			  (
			    this 
			    + " returning from local get for location : "
			    + path
			    + "\nwith value : " 
			    + v
			  )
			)

			value match {
			  case RBound(
			    Some( Ground( gv ) ),
			    Some( soln ) 
			  ) => {
			    tweet(
			      (
				this 
				+ " sending value "
				+ v
				+ " back "
			      )
			    )

			    smajatp.send(
			      Msgs.MDGetResponse[Namespace,Var,Tag,Value](
				path,
				gv
			      )
			    )
			  }
			  case Ground( gv ) => {
			    tweet(
			      (
				this 
				+ " sending value "
				+ v
				+ " back "
			      )
			    )

			    smajatp.send(
			      Msgs.MDGetResponse[Namespace,Var,Tag,Value](
				path,
				gv
			      )
			    )
			  }
			  case _ => {
			    tweet(
			      (
				this 
				+ " not sending composite value "
				+ v
				+ " back "
			      )
			    )
			  }
			}
		      }
		      v
		    }
		  }
		tweet(
		  (
		    this 
		    + "calling get locally for location : "
		    + path
		  )
		)
		get( List( msrc ) )( path, k )
	      }
	      case dfreq@Msgs.MDFetchRequest( path ) => {
		tweet(
		  (
		    this 
		    + "handling : "
		    + dfreq
		  )
		)
		val k =
		  {
		    ( v : Option[Resource] ) => {
		      //tap( v )
		      for(
			atp <- agentTwistedPairs.get( msrc );
			value <- v
		      ) {
			val smajatp : SMAJATwistedPair =
			  atp.asInstanceOf[SMAJATwistedPair]

			value match {
			  case Ground( gv ) =>
			    smajatp.send(
			      Msgs.MDGetResponse[Namespace,Var,Tag,Value](
				path,
				gv
			      )
			    )
			}
		      }
		      v
		    }
		  }
		fetch( path, k )
	      }
	      case dpreq@Msgs.MDPutRequest( path, value ) => {	
		tweet(
		  (
		    this
		    + " handling : "
		    + dpreq
		  )
		)
		put( path, value )
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
	      put( path, value )
	    }
	    case Msgs.MDFetchResponse( path, value ) => {
	      put( path, value )
	    }
	    case dput : Msgs.MDPutResponse[Namespace,Var,Tag,Value] => {	
	    }
	    case _ => {
	      tweet(
		(
		  this 
		  + " handling unexpected message : "
		  + body
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
	  tweet(
	    (
	      this
	      + " handling : "
	      + dmsg
	      + " from "
	      + msrc
	      + " on behalf of "
	      + mtrgt
	    )
	  )
	  handleRequest( dreq )
	}
	case Right(
	  drsp@JustifiedResponse( 
	    msgId, mtrgt, msrc, lbl, body, _
	  )
	) => {
	  tweet(
	    (
	      this
	      + " handling : "
	      + dmsg
	      + " from "
	      + msrc
	      + " on behalf of "
	      + mtrgt
	    )
	  )
	  handleResponse( drsp )
	}
      }
    }

    def get( hops : List[URI] )(
      path : CnxnCtxtLabel[Namespace,Var,Tag],
      next : WhatNext
    )
    : Seq[Option[Resource]] = {        
      for( placeNSoln <- places( path, Input ) )
      yield {
	val ( place, soln ) = placeNSoln
	_labelMap.get( place ) match {
	  case sv @ Some( value ) => {
	    _labelMap -= place
	    next( sv )
	  }
	  case None => {
	    reset {
	      val rslt : Option[Resource] = 
		shift {
		  ( k : GetContinuation ) => {	      
		    tweet(
		      (
			this
			+ " storing continuation to wait for value : "
			+ k
		      )
		    )
		    _waiters( place ) =
		      _waiters.get( place )
		    .getOrElse( Nil ) ++ List( k )
		    
		    tweet(
		      (
			this 
			+ " forwarding to acquaintances "
		      )
		    )

		    forwardGet( hops, path )
		    
		    k( None )
		  }	    	      
		}
	      tweet(
		(
		  this
		  + " resuming with value : "
		  + rslt
		)
	      )
	      rslt match {
		case Some( _ ) => next( rslt )
		case nv @ _ => nv
	      }
	    }
	  }	
	}
      }    
    }
  
    override def get(
      path : CnxnCtxtLabel[Namespace,Var,Tag],
      next : WhatNext
    )
    : Seq[Option[Resource]] = {        
      get( Nil )( path, next )    
    }
  }

  class MonadicJoin[PlaceT <: CnxnCtxtLabel[Namespace,Var,Tag]](
    override val name : URI,
    override val acquaintances : Seq[URI]
  ) extends InMemoryMonadicJunction(
    name,
    acquaintances
  ) with FJTaskRunners
  with MonadicConcurrentGenerators {

    case class Waiting(
      path : CnxnCtxtLabel[Namespace,Var,Tag],
      wk : GetContinuation
    ) extends Resource
    with Function1[Option[Resource],Option[Resource]] {
      override def apply( or : Option[Resource] ) : Option[Resource] = {
	wk( or )
      }
    }

    case class Rejoin(
      path : CnxnCtxtLabel[Namespace,Var,Tag],
      rsrc : Option[Resource],
      soln : Option[Solution[String]]
    ) extends Resource

    case class PartialResult(
      fulfilled : TMap[Namespace,Var,Tag,Value],
      outstanding : TMap[Namespace,Var,Tag,Value]
    ) extends Resource

    override def put(
      path : CnxnCtxtLabel[Namespace,Var,Tag],
      resource : Resource
    ) : Unit = {    
      for( placeNSoln <- places( path, Output ) ) {
	val ( place, soln ) = placeNSoln
	_waiters.get( place ) match {
	  case Some( k :: ks ) => {
	    _waiters( place ) = ks
	    val ans =
	      k match {
		case Waiting( p, _ ) => {
		  Rejoin( p, Some( resource ), soln )
		}
		case _ => {
		  RBound( Some( resource ), soln )
		}
	      }
	    k( Some( ans ) )
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

    def join( hops : List[URI] )(
      asks : Generator[PlaceT,Unit,Unit],
      fulfilled : TMap[Namespace,Var,Tag,Value],
      outstanding : TMap[Namespace,Var,Tag,Value]
    ) = 
      Generator {
	k : ( Option[Resource] => Unit @suspendable ) =>
	  shift {
	    outerK : ( Unit => Unit ) =>
	      reset {
		for ( ask <- asks ) {
		  val oRsrc =
		    reset {
		      val rslt : Option[Resource] =
			shift {
			  ( wk : GetContinuation ) => {		  
			    tweet(
			      (
				this
				+ " storing continuation "
				+ wk + " to wait for values "
				+ asks
			      )
			    )
			    
			    val stillWaiting = Waiting( ask, wk )
			    
			    outstanding( ask ) = stillWaiting
			    
			    _waiters( ask ) =
			      _waiters.get( ask )
			    .getOrElse( Nil ) ++ List( stillWaiting )
			    
			    forwardGet( hops, ask )
			    
			    stillWaiting(
			      Some(
				PartialResult( fulfilled, outstanding )
			      )
			    )
			  }		  		  
			}
		      
		      tweet(
			(
			  this
			  + " resuming with value : "
			  + rslt
			)
		      )
		      
		      rslt match {
			case Some( PartialResult( _, _ ) ) => rslt
			case Some( Rejoin( p, rsrc, soln ) ) => {
			  fulfilled( p ) = RBound( rsrc, soln )
			  outstanding -= p
			  if ( outstanding.isEmpty ) {
			    Some( RMap( fulfilled ) )
			  }
			  else {		
			    Some( PartialResult( fulfilled, outstanding ) )
			  }
			}		
			case Some( _ ) => rslt
			case _ => rslt
		      }
		    }

		  tweet( "join resuming with result: " + oRsrc )

		  k( oRsrc )
		}

		tweet( "join returning" )
		outerK()
	      }
	  }
      }

    def serve( hops : List[URI] )(
      placePatterns : Seq[PlaceT] // TODO replace with Generator which
				  // implies adding flatMap to Generator
    ) = Generator {
      k : ( Option[Resource] => Unit @suspendable ) =>
	tweet(
	  "Agent is serving now... "
	)
      val locations =
	placePatterns.flatMap(
	  ( plptn ) => places( plptn, Input )
	)

      val fulfilled = new TMap[Namespace,Var,Tag,Value]()
      val outstanding = new TMap[Namespace,Var,Tag,Value]()
      
      val ( asksNVals, answersNVals ) =
	locations.partition(
	  ( placeNVal ) => {
	    val ( l, _ ) = placeNVal
	    _labelMap.get( l ) match {
	      case Some( v ) => {
		// consume the value from the _labelMap
		_labelMap -= l
		// and cache it for the time when we have
		// all the values
		fulfilled( l ) = v
		false
	      }
	      case None => {		
		// sadly or not we cannot mark the outstanding place
		// until we have the continuation
		true
	      }
	    }
	  }
	)

      val asks = asksNVals.unzip._1

      asks match {
	case a :: rasks => {	  	  	  	  
	  for(
	    j <- spawnGen[Option[Resource]](
	      join( hops )( itergen( asks.asInstanceOf[Iterable[PlaceT]] ), fulfilled, outstanding )
	    )
	  ) {
	    k( j )
	  }
	}
	case Nil => {
	  k( Some( RMap( fulfilled ) ) )
	}
      }      
    }

  }
  
}


/* ------------------------------------------------------------------
 * Mostly self-contained object to support unit testing
 * ------------------------------------------------------------------ */ 

object MonadicMsgJunction
  extends MonadicDTSMsgScope[String,String,String,String]
  with UUIDOps
{
  type MsgTypes = DTSMSH[String,String,String,String]

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
