// -*- mode: Scala;-*- 
// Filename:    MonadicKVDBNode.scala 
// Authors:     lgm                                                    
// Creation:    Mon Feb 27 18:51:20 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._
import net.liftweb.amqp._

import scala.util.continuations._ 
import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

import com.rabbitmq.client._

import org.prolog4j._

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.util.UUID
import java.net.URI
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream

trait MonadicKVDBNodeScope[Namespace,Var,Tag,Value]
extends MonadicSoloTermStoreScope[Namespace,Var,Tag,Value] 
{
  trait DTSMSHRsrc extends DTSMSH[Namespace,Var,Tag,Value] {
    trait RsrcResponse[Namespace,Var,Tag,Value]
    case class MDGetResponseRsrc[Namespace,Var,Tag,Value](
      path : CnxnCtxtLabel[Namespace,Var,Tag],
      rsrc : mTT.Resource
    ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
	 with RsrcResponse[Namespace,Var,Tag,Value]
    case class MDFetchResponseRsrc[Namespace,Var,Tag,Value](
      path : CnxnCtxtLabel[Namespace,Var,Tag],
      rsrc : mTT.Resource
    ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
	 with RsrcResponse[Namespace,Var,Tag,Value]
    case class MDSubscribeResponseRsrc[Namespace,Var,Tag,Value](
      path : CnxnCtxtLabel[Namespace,Var,Tag],
      rsrc : mTT.Resource
    ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
	 with RsrcResponse[Namespace,Var,Tag,Value]
    case class MDPutResponseRsrc[Namespace,Var,Tag,Value](
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
	 with RsrcResponse[Namespace,Var,Tag,Value]
    case class MDPublishResponseRsrc[Namespace,Var,Tag,Value](
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
	 with RsrcResponse[Namespace,Var,Tag,Value]    
  }

  type RsrcMsgTypes <: DTSMSHRsrc
  override type MsgTypes = RsrcMsgTypes
  def protoRsrcMsgs : RsrcMsgTypes
  val RsrcMsgs : RsrcMsgTypes = protoRsrcMsgs

  type KVDBNodeRequest = Msgs.MDistributedTermSpaceRequest[Namespace,Var,Tag,Value]
  type KVDBNodeResponse = RsrcMsgs.RsrcResponse[Namespace,Var,Tag,Value]

  abstract class AbstractMonadicKVDB[ReqBody, RspBody](
    override val name : Moniker
  ) extends Individual[ReqBody,RspBody,AbstractMonadicKVDBNode](
    name,
    new ListBuffer[JustifiedRequest[ReqBody,RspBody]](),
    new ListBuffer[JustifiedResponse[ReqBody,RspBody]]()
  ) with MonadicTermStoreT
  {    
  }
  
  abstract class AbstractMonadicKVDBNode[ReqBody, RspBody](
    val localCache : AbstractMonadicKVDB[ReqBody,RspBody],
    override val acquaintances : List[Moniker]
  ) extends MonadicTxPortFramedMsgDispatcher[String,ReqBody,RspBody,AbstractMonadicKVDBNode](
    localCache, acquaintances
  ) with MonadicTermStoreT {
    import identityConversions._
  
    def txPort2FramedMsg [A <: FramedMsg] ( txPortMsg : String ) : A = {
      val xstrm = new XStream( new JettisonMappedXmlDriver )
      xstrm.fromXML( txPortMsg ).asInstanceOf[A]
    }
    def framedMsg2TxPort [A >: FramedMsg] ( txPortMsg : A ) : String = {
      val xstrm = new XStream( new JettisonMappedXmlDriver )
      xstrm.toXML( txPortMsg )
    }
  }

  case class MonadicKVDB(
    override val name : Moniker
  ) extends AbstractMonadicKVDB[KVDBNodeRequest,KVDBNodeResponse](
    name
  ) {
    override def configFileName : Option[String] = None
    override def configurationDefaults : ConfigurationDefaults = {
      ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
    } 
  }
  case class MonadicKVDBNode(
    cache : MonadicKVDB,
    override val acquaintances : List[Moniker]
  ) extends AbstractMonadicKVDBNode[KVDBNodeRequest,KVDBNodeResponse](
    cache, acquaintances
  ) {    

    def wrapResponse(
      msrc : Moniker, dreq : Msgs.DReq, rsrc : mTT.Resource
    ) : FramedMsg = {
      frameResponse( msrc )(
	dreq match {
	  case Msgs.MDGetRequest( path ) => {
	    RsrcMsgs.MDGetResponseRsrc[Namespace,Var,Tag,Value]( path, rsrc )	  
	  }
	  case Msgs.MDFetchRequest( path ) => {
	    RsrcMsgs.MDFetchResponseRsrc[Namespace,Var,Tag,Value]( path, rsrc )
	  }
	  case Msgs.MDSubscribeRequest( path ) => {
	    RsrcMsgs.MDSubscribeResponseRsrc[Namespace,Var,Tag,Value]( path, rsrc )
	  }
	  case _ => {
	    throw new Exception( "unexpected request type " + dreq )
	  }	  
	}
      )
    }

    def wrapResponse(
      msrc : Moniker, dreq : Msgs.DReq
    ) : FramedMsg = {
      frameResponse( msrc )(
	dreq match {	  
	  case Msgs.MDPutRequest( path, _ ) => {
	    RsrcMsgs.MDPutResponseRsrc[Namespace,Var,Tag,Value]( path )
	  }
	  case Msgs.MDPublishRequest( path, _ ) => {
	    RsrcMsgs.MDPublishResponseRsrc[Namespace,Var,Tag,Value]( path )
	  }
	  case _ => {
	    throw new Exception( "unexpected request type " + dreq )
	  }
	}
      )
    }

    def handleValue( dreq : Msgs.DReq, oV : Option[mTT.Resource], msrc : Moniker ) : Unit = {
      for( q <- stblQMap.get( msrc ); value <- oV ) {	
	tweet( ( this + " sending value " + oV + " back " ) )	   
	q ! wrapResponse( msrc, dreq, value )
      }
    }

    def dispatchDMsg( dreq : FramedMsg ) : Unit = {
      val Left( JustifiedRequest( msgId, mtrgt, msrc, lbl, body, _ ) ) = dreq

      body match {
	case dgreq@Msgs.MDGetRequest( path ) => {	  
	  tweet( ( this + "getting locally for location : " + path ) )
	  reset {
	    for( v <- get( List( msrc ) )( false )( path ) ) {
	      tweet(
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
	  tweet( ( this + "fetching locally for location : " + path ) )
	  reset {
	    for( v <- fetch( List( msrc ) )( false )( path ) ) {
	      tweet(
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
	  tweet( ( this + "subscribing locally for location : " + path ) )
	  reset {
	    for( v <- subscribe( List( msrc ) )( path ) ) {
	      tweet(
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
	  reset { cache.put( path, mTT.Ground( value ) ) }
	  for( q <- stblQMap.get( msrc ) ) {
	    q ! wrapResponse( msrc, dpreq )
	  }
	}
	case dpbreq@Msgs.MDPublishRequest( path, value ) => {	
	  reset { cache.publish( path, mTT.Ground( value ) ) }
	  for( q <- stblQMap.get( msrc ) ) {
	    q ! wrapResponse( msrc, dpbreq )
	  }
	}
      }
    }

    def dispatchDMsgs()  : Unit = {
      reset {
	for( dreq <- ??() ) {
	  tweet( this + "handling : " + dreq )	
	  dispatchDMsg( dreq )
	}
      }
    }

    def forward(
      ask : dAT.AskNum,
      hops : List[Moniker],
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Unit = {

      tweet( ( this + " in forwardGet with hops: " + hops ) )

      for( trgt <- acquaintances; q <- stblQMap.get( trgt ) if !hops.contains( trgt ) ) {
	tweet( ( this + " forwarding to " + trgt ) )
	val request : KVDBNodeRequest = 
	  ask match {
	    case dAT.AGetNum => {
	      Msgs.MDGetRequest[Namespace,Var,Tag,Value](
		path
	      )
	    }
	    case dAT.AFetchNum => {
	      Msgs.MDFetchRequest[Namespace,Var,Tag,Value](
		path
	      )
	    }
	    case dAT.ASubscribeNum => {
	      Msgs.MDSubscribeRequest[Namespace,Var,Tag,Value](
		path
	      )
	    }
	  }

	q ! frameRequest( trgt )( request )
      }
    }

    def mget( ask : dAT.AskNum, hops : List[Moniker] )(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : Boolean,
      cursor : Boolean
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
		  oV <- cache.mget( channels, registered, consume )( path ) 
		) {
		  oV match {
		    case None => {
		      tweet( ">>>>> forwarding..." )
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

    def get( hops : List[Moniker] )( cursor : Boolean )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Generator[Option[mTT.Resource],Unit,Unit] = {              
      mget( dAT.AGetNum, hops )( theMeetingPlace, theWaiters, true, cursor )( path )    
    }
    
    def get( cursor : Boolean )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ): Generator[Option[mTT.Resource],Unit,Unit] = {
      get( Nil )( cursor )( path )
    }    
    
    override def get(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Generator[Option[mTT.Resource],Unit,Unit] = {        
      get( Nil )( false )( path )    
    }
    
    def fetch( hops : List[Moniker] )(
      cursor : Boolean
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {              
      mget( dAT.AFetchNum, hops )(
	theMeetingPlace, theWaiters, false, cursor
      )( path )    
    }
    
    def fetch(
      cursor : Boolean
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {
      get( Nil )( cursor )( path )
    }
    
    override def fetch(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      fetch( Nil )( false )( path )    
    }

    def subscribe( hops : List[Moniker] )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      mget( dAT.ASubscribeNum, hops )(
	theChannels, theSubscriptions, true, false
      )( path )    
    }
    
    override def subscribe(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      subscribe( Nil )( path )    
    }

    override def configFileName : Option[String] = None
    override def configurationDefaults : ConfigurationDefaults = {
      ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
    } 
  }

  object KVDBNodeFactory extends AMQPURIOps with FJTaskRunners {
    def ptToPt( here : URI, there : URI ) : MonadicKVDBNode = {
      val node = MonadicKVDBNode( MonadicKVDB( MURI( here ) ), List( MURI( there ) ) )
      spawn { node.dispatchDMsgs() }
      node
    }
    def loopBack( here : URI ) : MonadicKVDBNode = {
      val exchange = uriExchange( here )
      val hereNow =
	new URI(
	  here.getScheme,
	  here.getUserInfo,
	  here.getHost,
	  here.getPort,
	  "/" + exchange + "Local",
	  here.getQuery,
	  here.getFragment
	)
      val thereNow =
	new URI(
	  here.getScheme,
	  here.getUserInfo,
	  here.getHost,
	  here.getPort,
	  "/" + exchange + "Remote",
	  here.getQuery,
	  here.getFragment
	)
      val node = MonadicKVDBNode( MonadicKVDB( MURI( hereNow ) ), List( MURI( thereNow ) ) )
      spawn { node.dispatchDMsgs() }
      node
    }
  }
}
