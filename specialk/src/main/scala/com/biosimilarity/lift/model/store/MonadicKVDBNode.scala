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
//import scala.concurrent.cpsops._
import scala.collection.mutable.Map
import scala.collection.mutable.MapProxy
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

  abstract class AbstractMonadicKVDB[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse, +Node[Rq <: ReqBody,Rs <: RspBody] <: AbstractMonadicKVDBNode[Rq,Rs,Node]](
    override val name : Moniker
  ) extends Individual[ReqBody,RspBody,Node](
    name,
    new ListBuffer[JustifiedRequest[ReqBody,RspBody]](),
    new ListBuffer[JustifiedResponse[ReqBody,RspBody]]()
  ) with MonadicTermStoreT
  {    
  }

  abstract class AbstractMonadicKVDBNode[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse, +Node[Rq <: ReqBody,Rs <: RspBody] <: AbstractMonadicKVDBNode[Rq,Rs,Node]](
    val localCache : AbstractMonadicKVDB[ReqBody,RspBody,Node],
    override val acquaintances : List[Moniker]
  ) extends MonadicTxPortFramedMsgDispatcher[String,ReqBody,RspBody,Node](
    localCache, acquaintances
  ) with MonadicTermStoreT {    
    self : MessageFraming[String,ReqBody,RspBody] =>
  
    import identityConversions._
    override def txPort2FramedMsg [A <: FramedMsg] ( txPortMsg : String ) : A = {
      //BasicLogService.tweet( "unwrapping transport message : " + txPortMsg )
      // BUGBUG -- lgm : there's a bug in the JettisonMappedXmlDriver
      // that misses the option declaration inside the RBound subtype
      // of Resource; so, the workaround is to use XML instead of JSON
      //val xstrm = new XStream( new JettisonMappedXmlDriver )
      val xstrm = new XStream( )
      val fmsg = xstrm.fromXML( txPortMsg )
      //BasicLogService.tweet( "resulting framed message : " + fmsg )
      fmsg.asInstanceOf[A]
    }
    override def framedMsg2TxPort [A >: FramedMsg] ( txPortMsg : A ) : String = {
      //BasicLogService.tweet( "wrapping framed message : " + txPortMsg )
      //val xstrm = new XStream( new JettisonMappedXmlDriver )
      val xstrm = new XStream( )
      val xmsg = xstrm.toXML( txPortMsg )
      //BasicLogService.tweet( "resulting transport message : " + xmsg )
      xmsg
    }
  }  

  class BaseMonadicKVDB[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse, +Node[Rq <: ReqBody, Rs <: RspBody] <: BaseMonadicKVDBNode[Rq,Rs,Node]](
    override val name : Moniker
  ) extends AbstractMonadicKVDB[ReqBody,RspBody,Node](
    name
  ) {
    override def configFileName : Option[String] = None
    override def configurationDefaults : ConfigurationDefaults = {
      ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
    } 
    override def toString() : String = {
      (
	this.getClass.getName.split( "\\." ).last + "@"
	+ ( name match { case MURI( uri ) => uri; case _ => name } )
      )
    }
    override def equals( o : Any ) : Boolean = {
      o match {
	case that : BaseMonadicKVDB[ReqBody,RspBody,Node] => {
	  (
	    name.equals( that.name ) 
	  )
	}
	case _ => false
      }
    }
    override def hashCode( ) : Int = {
      (
	( 37 * name.hashCode )
      )
    }
  }

  object BaseMonadicKVDB {
    def apply [ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse, Node[Rq <: ReqBody, Rs <: RspBody] <: BaseMonadicKVDBNode[Rq,Rs,Node]]( name : Moniker ) : BaseMonadicKVDB[ReqBody,RspBody,Node] = {
      new BaseMonadicKVDB[ReqBody,RspBody,Node]( name )
    }
    def unapply [ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse,Node[Rq <: ReqBody, Rs <: RspBody] <: BaseMonadicKVDBNode[Rq,Rs,Node]]( mkvdb : BaseMonadicKVDB[ReqBody,RspBody,Node] ) : Option[(Moniker)] = {
      Some( ( mkvdb.name ) )
    }
  }

  class BaseMonadicKVDBNode[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse,+Node[Rq <: ReqBody, Rs <: RspBody] <: BaseMonadicKVDBNode[Rq,Rs,Node]](
    val cache : BaseMonadicKVDB[ReqBody,RspBody,Node],
    override val acquaintances : List[Moniker]
  ) extends AbstractMonadicKVDBNode[ReqBody,RspBody,Node](
    cache, acquaintances
  ) with MessageFraming[String,ReqBody,RspBody] {    
    override def toString() : String = {
      (
	this.getClass.getName.split( "\\." ).last + "@"
	+ ( name match { case MURI( uri ) => uri; case _ => name } )
      )
    }
    override def equals( o : Any ) : Boolean = {
      o match {
	case that : BaseMonadicKVDBNode[ReqBody,RspBody,Node] => {
	  (
	    cache.equals( that.cache ) 
	    && acquaintances.equals( that.acquaintances )
	  )
	}
	case _ => false
      }
    }
    override def hashCode( ) : Int = {
      (
	( 37 * cache.hashCode )
	+ ( 37 * acquaintances.hashCode )
      )
    }
    def wrapResponse(
      msrc : Moniker, dreq : Msgs.DReq, rsrc : mTT.Resource
    ) : FramedMsg = {
      val rsp = 
	dreq match {
	  case Msgs.MDGetRequest( path ) => {
	    RsrcMsgs.MDGetResponseRsrc[Namespace,Var,Tag,Value]( path, mTT.portRsrc( rsrc, path ) )	  
	  }
	  case Msgs.MDFetchRequest( path ) => {
	    RsrcMsgs.MDFetchResponseRsrc[Namespace,Var,Tag,Value]( path, mTT.portRsrc( rsrc, path ) )
	  }
	  case Msgs.MDSubscribeRequest( path ) => {
	    RsrcMsgs.MDSubscribeResponseRsrc[Namespace,Var,Tag,Value]( path, mTT.portRsrc( rsrc, path ) )
	  }
	  case _ => {
	    throw new Exception( "unexpected request type " + dreq )
	  }	  
	}
      
      rsp match {
	case rsbdy : RspBody => {
	  frameResponse( msrc )( rsbdy )
	}
	case _ => {
	  throw new Exception( "unable to frame response: " + rsp )
	}
      }
    }

    def wrapResponse(
      msrc : Moniker, dreq : Msgs.DReq
    ) : FramedMsg = {
      val rsp = 
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
      rsp match {
	case rsbdy : RspBody => {
	  frameResponse( msrc )( rsbdy )
	}
	case _ => {
	  throw new Exception( "unable to frame response: " + rsp )
	}
      }
    }    

    def handleValue( dreq : Msgs.DReq, oV : Option[mTT.Resource], msrc : Moniker ) : Unit = {           	   
      BasicLogService.tweet(
	(
	  "BaseAgentKVDBNode : "
	  + "\nmethod : handleValue "
	  + "\nthis : " + this
	  + "\n dreq : " + dreq
	  + "\n oV : " + oV
	  + "\n msrc : " + msrc
	  + "\n------------------"
	)
      )

      stblQMap.get( msrc ) match {        
        case Some( q ) => {
          BasicLogService.tweet(
	    (
	      "BaseAgentKVDBNode : "
	      + "\nmethod : handleValue "
	      + "\nthis : " + this
	      + "\n dreq : " + dreq
	      + "\n oV : " + oV
	      + "\n msrc : " + msrc
	      + "\n------------------"
	      + "\n q : " + q
	    )
          )
          for( value <- oV ) {
            BasicLogService.tweet( ( this + " sending value " + oV + " back " ) )
	    q ! wrapResponse( msrc, dreq, value )
          }
        }
        case None => {
          val msrcKey : URM =
	    msrc match {
	      case msrcMURI : MURI =>
	        com.biosimilarity.lift.lib.moniker.identityConversions.toURM( msrcMURI )
	      case msrcURM : URM => msrcURM
	      case _ => throw new Exception( "unexpected msrc type: " + msrc.getClass )
	    }
      
          BasicLogService.tweet(
	    (
	      "BaseAgentKVDBNode : "
	      + "\nmethod : handleValue "
	      + "\nthis : " + this
	      + "\n dreq : " + dreq
	      + "\n oV : " + oV
	      + "\n msrc : " + msrc
	      + "\n------------------"
              + "\n msrcKey : " + msrcKey
	      + "\n q : " + stblQMap.get( msrcKey )
	    )
          )
          for( q <- stblQMap.get( msrcKey ); value <- oV ) {	
	    BasicLogService.tweet( ( this + " sending value " + oV + " back " ) )
	    q ! wrapResponse( msrc, dreq, value )
          }
        }
      }      
    }

    def dispatchDMsg( dreq : FramedMsg ) : Unit = {
      BasicLogService.tweet(
	(
	  "BaseMonadicKVDBNode : "
	  + "\nmethod : dispatchDMsg "
	  + "\nthis : " + this
	  + "\ndreq : " + dreq
	)
      )
      dreq match {
	case Left( JustifiedRequest( msgId, mtrgt, msrc, lbl, body, _ ) ) => {
	  body match {
	    case dgreq@Msgs.MDGetRequest( path ) => {	  
	      BasicLogService.tweet(
		(
		  "BaseMonadicKVDBNode : "
		  + this
		  + " getting locally for location : "
		  + path
		)
	      )
	      reset {
		for( v <- get( List( msrc ) )( false )( path ) ) {
		  BasicLogService.tweet(
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
	      BasicLogService.tweet( ( this + "fetching locally for location : " + path ) )
	      reset {
		for( v <- fetch( List( msrc ) )( false )( path ) ) {
		  BasicLogService.tweet(
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
	      BasicLogService.tweet( ( this + "subscribing locally for location : " + path ) )
	      reset {
		for( v <- subscribe( List( msrc ) )( path ) ) {
		  BasicLogService.tweet(
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
	// BUGBUG -- lgm : DRY this please
	case Right( JustifiedResponse( msgId, mtrgt, msrc, lbl, body, _ ) ) => {
	  body match {
	    case RsrcMsgs.MDGetResponseRsrc( path, rsrc ) => {
	      rsrc match {
		// if the rsrc comes with a substitution
		// apply that to the path
		case rbnd : mTT.RBound => {
		  rsrc( path ) match {
		    // if the application results in a specialization
		    // put the results there
		    case Some( spec ) => {
		      for( inrRsrc <- rbnd.rsrc ) {
			reset { cache.put( spec, inrRsrc ) }
		      }		      		  
		    }
		    // else put the results at the path
		    case None => {
		      reset { cache.put( path, rsrc ) }
		    }
		  }		  
		}
		case _ => {
		  reset { cache.put( path, rsrc ) }
		}
	      }	      
	    }
	    case RsrcMsgs.MDFetchResponseRsrc( path, rsrc ) => {
	      rsrc match {
		case rbnd : mTT.RBound => {
		  rsrc( path ) match {
		    case Some( spec ) => {
		      for( inrRsrc <- rbnd.rsrc ) {
			reset { cache.put( spec, inrRsrc ) }
		      }		      		  
		    }
		    case None => {
		      reset { cache.put( path, rsrc ) }
		    }
		  }		  
		}
		case _ => {
		  reset { cache.put( path, rsrc ) }
		}
	      }
	    }
	    case RsrcMsgs.MDSubscribeResponseRsrc( path, rsrc ) => {
	      rsrc match {
		case rbnd : mTT.RBound => {
		  rsrc( path ) match {
		    case Some( spec ) => {
		      for( inrRsrc <- rbnd.rsrc ) {
			reset { cache.publish( spec, inrRsrc ) }
		      }		      		  
		    }
		    case None => {
		      reset { cache.publish( path, rsrc ) }
		    }
		  }		  
		}
		case _ => {
		  reset { cache.publish( path, rsrc ) }
		}
	      }
	    }	    
	    case dput : RsrcMsgs.MDPutResponse[Namespace,Var,Tag,Value] => {	
	    }
	    case _ => {
	      BasicLogService.tweet(
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

    def mkGetRsp( path : CnxnCtxtLabel[Namespace,Var,Tag], rsrc : mTT.Resource ) = {
      RsrcMsgs.MDGetResponseRsrc[Namespace,Var,Tag,Value]( path, rsrc )
    }

    def dispatchDMsgs()  : Unit = {
      reset {
	for( dreq <- ??() ) {
	  BasicLogService.tweet( "BaseMonadicKVDBNode: " + this + " handling : " + dreq + " calling dispatchDMsg " )
	  dispatchDMsg( dreq )
	}
      }
    }

    def forward(
      ask : dAT.AskNum,
      hops : List[Moniker],
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Unit = {

      BasicLogService.tweet( ( this + " in forwardGet with hops: " + hops ) )

      for( trgt <- acquaintances; q <- stblQMap.get( trgt ) if !hops.contains( trgt ) ) {	
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

	request match {
	  case rqbdy : ReqBody => {
	    val framedReq = frameRequest( trgt )( rqbdy )
	    BasicLogService.tweet( ( this + " forwarding " + framedReq + " to " + trgt ) )
	    q ! framedReq
	  }
	  case _ => {
	    throw new Exception( "unable to frame request: " + request )
	  }
	}	
      }
    }

    def mget( ask : dAT.AskNum, hops : List[Moniker] )(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : RetentionPolicy,
      keep : RetentionPolicy,
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
		  oV <- cache.mget( channels, registered, consume, keep )( path ) 
		) {
		  oV match {
		    case None => {
		      //BasicLogService.tweet( ">>>>> forwarding..." )
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
      mget( dAT.AGetNum, hops )( cache.theMeetingPlace, cache.theWaiters, Cache, Cache, cursor )( path )    
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
	cache.theMeetingPlace, cache.theWaiters, DoNotRetain, DoNotRetain, cursor
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
	cache.theChannels, cache.theSubscriptions, Cache, Cache, false
      )( path )    
    }
    
    override def subscribe(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      subscribe( Nil )( path )    
    }

    override def put( ptn : CnxnCtxtLabel[Namespace,Var,Tag], rsrc : mTT.Resource ) = {
      //mput( cache.theMeetingPlace, cache.theWaiters, false )( ptn, rsrc )
      cache.mput( cache.theMeetingPlace, cache.theWaiters, false )( ptn, rsrc )
    }
    override def publish( ptn : CnxnCtxtLabel[Namespace,Var,Tag], rsrc : mTT.Resource ) = {
      //mput( cache.theChannels, cache.theSubscriptions, true )( ptn, rsrc )
      cache.mput( cache.theChannels, cache.theSubscriptions, true )( ptn, rsrc )
    }

    override def configFileName : Option[String] = None
    override def configurationDefaults : ConfigurationDefaults = {
      ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
    } 
  }

  object BaseMonadicKVDBNode {
    def apply [ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse,Node[Rq <: ReqBody, Rs <: RspBody] <: BaseMonadicKVDBNode[Rq,Rs,Node]] ( cache : BaseMonadicKVDB[ReqBody,RspBody,Node], acquaintances : List[Moniker] ) : BaseMonadicKVDBNode[ReqBody,RspBody,Node] = {
      new BaseMonadicKVDBNode[ReqBody,RspBody,Node]( cache, acquaintances )
    }
    def unapply [ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse,Node[Rq <: ReqBody, Rs <: RspBody] <: BaseMonadicKVDBNode[Rq,Rs,Node]] ( mkvdbnode : BaseMonadicKVDBNode[ReqBody,RspBody,Node] ) : Option[( BaseMonadicKVDB[ReqBody,RspBody,Node], List[Moniker] )] = {
      Some( ( mkvdbnode.cache, mkvdbnode.acquaintances ) )
    }
  }

  case class MonadicKVDB[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse](
    override val name : Moniker
  ) extends BaseMonadicKVDB[ReqBody,RspBody,MonadicKVDBNode](
    name
  )

  case class MonadicKVDBNode[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse](
    override val cache : MonadicKVDB[ReqBody,RspBody],
    override val acquaintances : List[Moniker]
  ) extends BaseMonadicKVDBNode[ReqBody,RspBody,MonadicKVDBNode](
    cache, acquaintances
  )

  object KVDBNodeFactory
    extends AMQPURIOps
    with ThreadPoolRunnersX
    //with FJTaskRunnersX
  {
    def ptToPt[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse]( here : URI, there : URI ) : MonadicKVDBNode[ReqBody,RspBody] = {
      val node = MonadicKVDBNode[ReqBody,RspBody]( MonadicKVDB[ReqBody,RspBody]( MURI( here ) ), List( MURI( there ) ) )
      spawn { node.dispatchDMsgs() }
      node
    }
    def loopBack[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse]( here : URI ) : MonadicKVDBNode[ReqBody,RspBody] = {
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
      val node = MonadicKVDBNode[ReqBody,RspBody]( MonadicKVDB( MURI( hereNow ) ), List( MURI( thereNow ) ) )
      spawn { node.dispatchDMsgs() }
      node
    }
  }
}

package usage {
  object MonadicKVDBNet
       extends MonadicKVDBNodeScope[String,String,String,Double]
       with UUIDOps
  {
    import SpecialKURIDefaults._
    import identityConversions._

    type MTTypes = MonadicTermTypes[String,String,String,Double]
    object TheMTT extends MTTypes with Serializable
    override def protoTermTypes : MTTypes = TheMTT

    type DATypes = DistributedAskTypes
    object TheDAT extends DATypes with Serializable
    override def protoAskTypes : DATypes = TheDAT
    
    override type MsgTypes = DTSMSHRsrc   
    override type RsrcMsgTypes = DTSMSHRsrc   

    val protoDreqUUID = getUUID()
    val protoDrspUUID = getUUID()    
    
    lazy val aLabel = new CnxnCtxtLeaf[String,String,String]( Left( "a" ) )

    object MonadicDRsrcMsgs extends RsrcMsgTypes with Serializable {
      
      override def protoDreq : DReq = MDGetRequest( aLabel )
      override def protoDrsp : DRsp = MDGetResponse( aLabel, 0.0 )
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
    
    override def protoMsgs : MsgTypes = MonadicDRsrcMsgs
    override def protoRsrcMsgs : RsrcMsgTypes = MonadicDRsrcMsgs
  }
  
  object MolecularUseCase {
    import MonadicKVDBNet._
    import KVDBNodeFactory._

    implicit val retTwist : Boolean = false
    def setup[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse](
      localHost : String, localPort : Int,
      remoteHost : String, remotePort : Int
    )(
      implicit returnTwist : Boolean
    ) : Either[MonadicKVDBNode[ReqBody,RspBody],(MonadicKVDBNode[ReqBody,RspBody],MonadicKVDBNode[ReqBody,RspBody])] = {
      val ( localExchange, remoteExchange ) = 
	if ( localHost.equals( remoteHost ) && ( localPort == remotePort ) ) {
	  ( "/molecularUseCaseProtocolLocal", "/molecularUseCaseProtocolRemote" )	  
	}
	else {
	  ( "/molecularUseCaseProtocol", "/molecularUseCaseProtocol" )	  
	}

      if ( returnTwist ) {
	Right[MonadicKVDBNode[ReqBody,RspBody],(MonadicKVDBNode[ReqBody,RspBody],MonadicKVDBNode[ReqBody,RspBody])](
	  (
	    ptToPt(
	      new URI( "agent", null, localHost, localPort, localExchange, null, null ),
	      new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
	    ),
	    ptToPt(	      
	      new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null ),
	      new URI( "agent", null, localHost, localPort, localExchange, null, null )
	    )
	  )
	)
      }
      else {
	Left[MonadicKVDBNode[ReqBody,RspBody],(MonadicKVDBNode[ReqBody,RspBody],MonadicKVDBNode[ReqBody,RspBody])](
	  ptToPt(
	    new URI( "agent", null, localHost, localPort, localExchange, null, null ),
	    new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
	  )
	)
      }
    }

    object KinaseSpecifications {
      import scala.math._
      
      trait Kinase {
	def b : Boolean
	def i : Int
	def state : String
	def update( j : Int ) : ConcreteKinase 
      }
      case class RAF(
	b : Boolean, i : Int, state : String
      ) extends Kinase {
	override def update( j : Int ) : ConcreteKinase = RAF( b, j, state )
      }
      case class RAS(
	b : Boolean, i : Int, state : String
      ) extends Kinase {
	override def update( j : Int ) : ConcreteKinase = RAS( b, j, state )
      }
      case class MEK1(
	b : Boolean, i : Int, state : String
      ) extends Kinase {
	override def update( j : Int ) : ConcreteKinase = MEK1( b, j, state )
      }
      case class MEK2(
	b : Boolean, i : Int, state : String
      ) extends Kinase {
	override def update( j : Int ) : ConcreteKinase = MEK2( b, j, state )
      }
      case class MAPK(
	b : Boolean, i : Int, state : String
      ) extends Kinase {
	override def update( j : Int ) : ConcreteKinase = MAPK( b, j, state )
      }
      
      lazy val RAFProto : RAF = RAF( true, 0, "Phosphorylated" )
      lazy val RASProto : RAS = RAS( true, 0, "Phosphorylated" )
      lazy val MEK1Proto : MEK1 = MEK1( true, 0, "Phosphorylated" )
      lazy val MEK2Proto : MEK2 = MEK2( true, 0, "Phosphorylated" )      
      lazy val MAPKProto : MAPK = MAPK( true, 0, "Phosphorylated" )      

      def mkMolQry( kinase : Kinase ) : CnxnCtxtLabel[String,String,String] = {
	import CnxnConversionStringScope._
	kinase match {
	  case cc : ScalaObject with Product with Serializable => {
	    asCnxnCtxtLabel( cc )
	  }
	  case _ => throw new Exception( "non concrete kinase: " + kinase )
	}	
      }

      def mkMolPtn( molType : String ) : CnxnCtxtLabel[String,String,String] = {
	new CnxnCtxtBranch[String,String,String](
	  "comBiosimilarityLiftModelStoreUsageMolecularUseCase_KinaseSpecifications_" + molType,
	  List( 
	    new CnxnCtxtLeaf[String,String,String](
	      Right[String,String]( "B" )
	    ),
	    new CnxnCtxtLeaf[String,String,String](
	      Right[String,String]( "I" )
	    ),
	    new CnxnCtxtBranch[String,String,String](
	      "state",
	      List(
		new CnxnCtxtLeaf[String,String,String](
		  Left[String,String]( "Phosphorylated" )
		)
	      )
	    )
	  )
	)
      }

      type ConcreteKinase = Kinase with Product with Serializable

      lazy val molPtnMap : HashMap[ConcreteKinase,CnxnCtxtLabel[String,String,String]] = {	
	val map = new HashMap[ConcreteKinase,CnxnCtxtLabel[String,String,String]]()
	map += ( RAFProto -> mkMolPtn( "RAF" ) )
	map += ( RASProto -> mkMolPtn( "RAS" ) )
	map += ( MEK1Proto -> mkMolPtn( "MEK1" ) )
	map += ( MEK2Proto -> mkMolPtn( "MEK2" ) )
	map += ( MAPKProto -> mkMolPtn( "MAPK" ) )
	map
      }

      implicit lazy val cascade : Seq[ConcreteKinase] =
	List[ConcreteKinase](
	  RAFProto, RASProto, MEK1Proto, MEK2Proto, MAPKProto
	)

      implicit lazy val cascadeInitialState : List[( ConcreteKinase, Option[ConcreteKinase] )] = {
	cascade.zip( cascade.drop( 1 ).map( Some( _ ) ) ++ List( None ) ).toList
      }

      implicit lazy val initialKinaseToProduce : ConcreteKinase = {	
	cascade.head
      }	

      //def raf2RAS : Double = random * 100
      def raf2RAS : Double = .10 * 100
      //def ras2MEK1 : Double = random * 100
      def ras2MEK1 : Double = .20 * 100
      //def mek12MEK2 : Double = random * 100
      def mek12MEK2 : Double = .30 * 100
      //def mek22MAPK : Double = random * 100
      def mek22MAPK : Double = .40 * 100
      //def mapk2Protein : Double = random * 100            
      def mapk2Protein : Double = .50 * 100            

      lazy val cascadeTransitionMap : HashMap[ConcreteKinase,Double] = {
	val map = new HashMap[ConcreteKinase,Double]()
	map += ( RAFProto -> raf2RAS )
	map += ( RASProto -> ras2MEK1 )
	map += ( MEK1Proto -> mek12MEK2 )
	map += ( MEK2Proto -> mek22MAPK )
	map += ( MAPKProto -> mapk2Protein )
	map
      }

      lazy val RAFPtn : CnxnCtxtLabel[String,String,String] =
	molPtnMap( RAFProto )
      
      lazy val RASPtn : CnxnCtxtLabel[String,String,String] =
	molPtnMap( RASProto )

      lazy val MEK1Ptn : CnxnCtxtLabel[String,String,String] =
	molPtnMap( MEK1Proto )

      lazy val MEK2Ptn : CnxnCtxtLabel[String,String,String] =
	molPtnMap( MEK2Proto )
            
      lazy val MAPKPtn : CnxnCtxtLabel[String,String,String] =
	molPtnMap( MAPKProto )
    }

    import KinaseSpecifications._

    trait CellularEnvironment {
      def kinaseMap : HashMap[CnxnCtxtLabel[String,String,String],Double] 
      def amt [K <: CnxnCtxtLabel[String,String,String]] ( proto : K ) : Double = {
	kinaseMap.get( proto ).getOrElse( 0 )
      }      
    }
    
    case class Cytoplasm( kinaseMap : HashMap[CnxnCtxtLabel[String,String,String],Double] )
	 extends CellularEnvironment with MapProxy[CnxnCtxtLabel[String,String,String],Double] {
	   override def self = kinaseMap
	 }

    implicit lazy val cellCytoplasm : Cytoplasm = Cytoplasm( new HashMap[CnxnCtxtLabel[String,String,String],Double]() )    

    def supplyKinase[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse](
      kvdbNode : MonadicKVDBNode[ReqBody,RspBody],
      cellCytoplasm : Cytoplasm,
      kinase : ConcreteKinase,
      trigger : Double
    ) : Unit = {
      import scala.math._
      import CnxnConversionStringScope._
      import cnxnConversions._
      new Thread {
	override def run() : Unit = {
	  def loop( proto : ConcreteKinase, kns : ConcreteKinase, amt : Double, count : Int ) : Unit = {
	    val kinasePtn = molPtnMap( proto )
	    val kamt = cellCytoplasm.amt( kinasePtn )
	    if ( kamt < amt ) {
	      val inc = random * 25
	      val nkns = kns.update( count + 1 )
	      val nknsLoc = mkMolQry( nkns )
	      cellCytoplasm += ( kinasePtn -> ( kamt + inc ) )
	      reset { 
		BasicLogService.tweet(
		  (
		    "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		    + kvdbNode + "\n"
		    + "releasing an increment " + inc + " of " + proto
		    + " to " + nknsLoc + "\n"
		    + "loop count: " + count + "\n"
		    + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		  )
		)
		BasicLogService.tweet(
		  (
		    "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		    + kvdbNode + "\n"
		    + "meeting place: " + kvdbNode.theMeetingPlace + "\n"
		    + "cache meeting place: " + kvdbNode.cache.theMeetingPlace + "\n"
		    + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		  )
		)
		kvdbNode.put( nknsLoc, inc )
		BasicLogService.tweet(
		  (
		    "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		    + kvdbNode + "\n"
		    + "meeting place: " + kvdbNode.theMeetingPlace + "\n"
		    + "cache meeting place: " + kvdbNode.cache.theMeetingPlace + "\n"
		    + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		  )
		)
	      }
	      loop( proto, nkns, amt, count + 1 )
	    }
	  }

	  loop( kinase, kinase, trigger, 0 )

	}
      }.start
    }

    def handleRsrc[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse](
      kvdbNode : MonadicKVDBNode[ReqBody,RspBody],
      cellCytoplasm : Cytoplasm,
      kinasePair : ( ConcreteKinase, Option[ConcreteKinase] )
    )(
      trigger : Double,
      inc : Double
    ) : Unit = {
      val ( kinaseToConsumeProto, optKinaseToProduceProto ) = kinasePair
      val kinaseToConsumeProtoPtn = molPtnMap( kinaseToConsumeProto )
      BasicLogService.tweet(
	(
	  "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	  + kvdbNode + "\n"
	  + "received an increment, "
	  + inc
	  + ", of "
	  + kinaseToConsumeProto + "\n"
	  + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	)
      )
      val currAmt : Double = cellCytoplasm.amt( kinaseToConsumeProtoPtn )
      val nAmt : Double = ( currAmt + inc )
      cellCytoplasm += ( ( kinaseToConsumeProtoPtn, nAmt ) )
      
      BasicLogService.tweet(
	(
	  "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	  + kvdbNode + "\n"
	  + "has accumulated " + nAmt + " of "
	  + kinaseToConsumeProto + "\n"
	  + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	)
      )
      
      optKinaseToProduceProto match {
	case Some( kinaseToProduceProto ) => {
	  for( amt <- cellCytoplasm.get( kinaseToConsumeProtoPtn ) ) {
	    // Got enough!
	    if ( amt > trigger ) {
	      BasicLogService.tweet( 
		(
		  "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		  + kvdbNode + "\n"
		  + "received enough "
		  + kinaseToConsumeProto
		  + " to produce "
		  + kinaseToProduceProto + "\n"
		  + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		)
	      )

	      for( nextTrigger <- cascadeTransitionMap.get( kinaseToProduceProto ) ) {
		BasicLogService.tweet(
		  (
		    "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		    + kvdbNode + "\n"
		    + "the trigger for the next transition is: " + nextTrigger + "\n"
		    + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		  )
		)
		
		// Supply some RAS
		supplyKinase(
		  kvdbNode,
		  cellCytoplasm,
		  kinaseToProduceProto,
		  nextTrigger
		)				
	      }
	    }
	    // Not quite enough...
	    else {
	      BasicLogService.tweet(
		(
		  "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		  + kvdbNode + "\n"
		  + "still waiting for enough "
		  + kinaseToConsumeProto
		  + " to produce "
		  + kinaseToProduceProto + "\n"
		  + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		)
	      )
	      processKinasePair(
		kvdbNode,
		cellCytoplasm,
		kinasePair
	      )
	    }
	  }		    		    
	}
	case _ => {
	  BasicLogService.tweet( 
	    (
	      "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	      + kvdbNode + "\n"
	      + "producing Protein.\n"
	      + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	    )
	  )
	}
      }		
    }
    
    def processKinasePair[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse](
      kvdbNode : MonadicKVDBNode[ReqBody,RspBody],
      cellCytoplasm : Cytoplasm,
      kinasePair : ( ConcreteKinase, Option[ConcreteKinase] )
    ) : Unit = {            
      BasicLogService.tweet( 
	(
	  "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	  + kvdbNode + "\n"
	  + "processing kinase pair "
	  + kinasePair + ".\n"
	  + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	)
      )

      val handleKinase = handleRsrc( kvdbNode, cellCytoplasm, kinasePair ) _

      val ( kinaseToConsumeProto, optKinaseToProduceProto ) = kinasePair
      val kinasePtn = molPtnMap( kinaseToConsumeProto )
      val trigger = cascadeTransitionMap.get( kinaseToConsumeProto ).getOrElse( java.lang.Double.MAX_VALUE )
      
      reset {
	// Wait for kinase
	BasicLogService.tweet(
	  (
	    "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	    + kvdbNode + "\n"
	    + "meeting place: " + kvdbNode.theMeetingPlace + "\n"
	    + "cache meeting place: " + kvdbNode.cache.theMeetingPlace + "\n"
	    + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	  )
	)
	for( kinaseRsrc <- kvdbNode.get( kinasePtn ) ) {
	  BasicLogService.tweet(
	    (
	      "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	      + kvdbNode + " received resource : " + kinaseRsrc + "\n"
	      + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	    )
	  )
	  BasicLogService.tweet(
	    (
	      "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	      + kvdbNode + "\n"
	      + "meeting place: " + kvdbNode.theMeetingPlace + "\n"
	      + "cache meeting place: " + kvdbNode.cache.theMeetingPlace + "\n"
	      + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
	    )
	  )
	  kinaseRsrc match {
	    // Got some!
	    case Some( mTT.RBoundAList( Some( mTT.Ground( inc ) ), soln ) ) => {
	      handleKinase( trigger, inc )
	    }
	    case Some( mTT.RBoundHM( Some( mTT.Ground( inc ) ), soln ) ) => {
	      handleKinase( trigger, inc )
	    }
	    case Some( mTT.RBoundHM( Some( mTT.RBoundAList( Some( mTT.Ground( inc ) ), innerSoln ) ), soln ) ) => {
	      handleKinase( trigger, inc )
	    }
	    case Some( mTT.Ground( inc ) ) => {
	      handleKinase( trigger, inc )
	    }
	    // Got none... so wait
	    case None => {
	      BasicLogService.tweet( 
		(
		  "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		  + kvdbNode + " received nothing; waiting for kinase, "
		  + kinaseToConsumeProto + ".\n"
		  + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		)
	      )
	    }
	    case unExpected@_ => {
	      throw new Exception( "Protocol violated. Received: " + unExpected )
	    }	    
	  }
	}
      }
    }        

    def runClient[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse]( kvdbNode : MonadicKVDBNode[ReqBody,RspBody] )( implicit cellCytoplasm : Cytoplasm ) : Unit = {
      import scala.math._
      import KinaseSpecifications._
      // map-reduce-style protocol checking      
      new Thread {
	override def run() : Unit = {
	  processKinasePair( kvdbNode, cellCytoplasm, cascadeInitialState( 0 ) )
	}
      }.start
      new Thread {
	override def run() : Unit = {
	  processKinasePair( kvdbNode, cellCytoplasm, cascadeInitialState( 2 ) )
	}
      }.start
      new Thread {
	override def run() : Unit = {
	  processKinasePair( kvdbNode, cellCytoplasm, cascadeInitialState( 4 ) )
	}
      }.start
    }

    def runServer[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse]( kvdbNode : MonadicKVDBNode[ReqBody,RspBody] )( implicit cellCytoplasm : Cytoplasm ) : Unit = {
      import scala.math._
      import KinaseSpecifications._
      // map-reduce-style protocol             
      new Thread {
	override def run() : Unit = {
	  supplyKinase( kvdbNode, cellCytoplasm, RAFProto, raf2RAS )
	}
      }.start
      new Thread {
	override def run() : Unit = {	  
	  processKinasePair( kvdbNode, cellCytoplasm, cascadeInitialState( 1 ) )
	}
      }.start
      new Thread {
	override def run() : Unit = {
	  processKinasePair( kvdbNode, cellCytoplasm, cascadeInitialState( 3 ) )
	}
      }.start
    }
    
  }
}
