// -*- mode: Scala;-*- 
// Filename:    MonadicNode.scala 
// Authors:     lgm                                                    
// Creation:    Fri Feb 17 09:24:00 2012 
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

trait MessageFraming[TxPort,ReqBody,RspBody] {
  self : UUIDOps =>
  import identityConversions._
  type FramedMsg = Either[JustifiedRequest[ReqBody,RspBody],JustifiedResponse[ReqBody,RspBody]]
  def name : Moniker
  def txPort2FramedMsg [A <: FramedMsg] ( txPortMsg : TxPort ) : A
  def framedMsg2TxPort [A >: FramedMsg] ( txPortMsg : A ) : TxPort
  implicit def requestJustification : Option[Response[AbstractJustifiedRequest[ReqBody,RspBody],RspBody]] = 
    None
  implicit def responseJustification : Option[Request[AbstractJustifiedResponse[ReqBody,RspBody],ReqBody]] = 
    None
  def frameRequest( trgt : Moniker )( req : ReqBody )(
    implicit just : Option[Response[AbstractJustifiedRequest[ReqBody,RspBody],RspBody]]
  ) : FramedMsg = {
    Left[JustifiedRequest[ReqBody,RspBody],JustifiedResponse[ReqBody,RspBody]](
      new JustifiedRequest[ReqBody,RspBody](
	getUUID(),
	toURI( trgt ),
	toURI( name ),
	getUUID(),
	req,
	just
      )
    )
  }
  def frameResponse( trgt : Moniker )( rsp : RspBody )(
    implicit just : Option[Request[AbstractJustifiedResponse[ReqBody,RspBody],ReqBody]]
  ) : FramedMsg = {
    Right[JustifiedRequest[ReqBody,RspBody],JustifiedResponse[ReqBody,RspBody]](
      new JustifiedResponse[ReqBody,RspBody](
	getUUID(),
	toURI( trgt ),
	toURI( name ),
	getUUID(),
	rsp,
	just
      )
    )
  }
  def body( fmsg : FramedMsg ) : Either[ReqBody,RspBody] = {
    fmsg match {
      case Left( jr : JustifiedRequest[ReqBody,RspBody] ) => {
	Left[ReqBody,RspBody]( jr.body)
      }
      case Right( jr : JustifiedResponse[ReqBody,RspBody] ) => {
	Right[ReqBody,RspBody]( jr.body )
      }
    }
  }
}

abstract class MonadicTxPortFramedMsgDispatcher[TxPort,ReqBody,RspBody,+SZ[Rq <: ReqBody, Rs <: RspBody] <: MonadicTxPortFramedMsgDispatcher[_,Rq,Rs,SZ]](
  override val individuality : Individual[ReqBody,RspBody,SZ],
  override val acquaintances : List[Moniker]
) extends RemoteSociety[ReqBody,RspBody,SZ](
  individuality, acquaintances
) with MonadicGenerators
  with MonadicConcurrentGenerators
  with ThreadPoolRunnersX
  //with FJTaskRunnersX
  with AMQPMonikerOps
  //with FJTaskRunners
  with UUIDOps
  with WireTap 
{
  self : MessageFraming[TxPort,ReqBody,RspBody] =>

  import identityConversions._

  case class AMQPTxPortFramedMsgScope(
    srcMnkr : Moniker,
    trgtMnkr : Moniker
  ) extends AMQPScope[TxPort](
    AMQPDefaults.defaultConnectionFactory
  ) with AMQPTwistedPairScope[TxPort] {
    override def src : URI = toURI( srcMnkr )
    override def trgt : URI = toURI( trgtMnkr )
    case class AMQPQueueTxPort2FramedMsgXForm[T](
      override val exchange : String,
      override val routingKey : String,    
      override val w2T : TxPort => T,
      override val t2W : T => TxPort,
      @transient override val dispatcherW : theMDS.Generator[TxPort,Unit,Unit],
      @transient override val senderW : theMDS.Generator[Unit,TxPort,Unit]
    ) extends AMQPQueueXForm[TxPort,T] 
    
    class TxPortOverAMQPQueueXFormM[A <: FramedMsg](
      val host : String,
      val port : Int,
      override val exchange : String,
      override val routingKey : String
    ) extends AMQPQueueMQT[A,AMQPQueueTxPort2FramedMsgXForm] {
      override def zero [B] : AMQPQueueTxPort2FramedMsgXForm[B] = {
	AMQPQueueTxPort2FramedMsgXForm[B](
	  exchange,
	  routingKey,	  
	  ( txPortMsg : TxPort ) => {
	    txPort2FramedMsg[FramedMsg]( txPortMsg ) match {
	      case b : B => b
	      case _ => {
		throw new Exception( "trgt is not a B" )
	      }
	    }
	  },
	  ( b : B ) => {
	    b match {
	      case trgt : FramedMsg => {
		framedMsg2TxPort[FramedMsg]( b.asInstanceOf[FramedMsg] )
	      }
	      case _ => {
		throw new Exception( "Not a trgt: " + b )
	      }
	    }
	  },
	  theMDS.serve[TxPort]( factory, host, port, exchange ),
	  theMDS.sender[TxPort]( host, port, exchange, routingKey )
	) 
      }
      def zeroFramedMsg [B >: FramedMsg] : AMQPQueueTxPort2FramedMsgXForm[B] = {
	AMQPQueueTxPort2FramedMsgXForm[B](
	  exchange,
	  routingKey,	  
	  txPort2FramedMsg[A],
	  framedMsg2TxPort[B],
	  theMDS.serve[TxPort]( factory, host, port, exchange ),
	  theMDS.sender[TxPort]( host, port, exchange, routingKey )
	)
      }    
    }

    class TxPortOverAMQPTwistedPairXForm[FMsg](
      override val tQP : TwistedQueuePair[TxPort]
    ) extends AMQPTwistedPairXForm[TxPort,FMsg](
      ( txPortMsg : TxPort ) => {
	//BasicLogService.tweet( "calling txPort2FramedMsg" )
	txPort2FramedMsg[FramedMsg]( txPortMsg ) match {
	  case fmsg : FMsg => {
	    BasicLogService.tweet( "fmsg : " + fmsg )
	    fmsg
	  }
	  case _ => {
	    throw new Exception( "trgt is not an FMsg" )
	  }
	}
      },
      ( fmsg : FMsg ) => {
	//BasicLogService.tweet( "calling framedMsg2TxPort" )
	fmsg match {
	  case trgt : FramedMsg => {
	    val txPortMsg = framedMsg2TxPort[FramedMsg]( fmsg.asInstanceOf[FramedMsg] )
	    //BasicLogService.tweet( "txPortMsg : " + txPortMsg )
	    txPortMsg
	  }
	  case _ => {
	    throw new Exception( "Not a trgt: " + fmsg )
	  }
	}
      },
      tQP
    ){
    }

    class TxPortOverAMQPTwistedQueuePairM[A <: FramedMsg](
      val srcMoniker : Moniker,
      val trgtMoniker : Moniker
    ) extends AMQPQueueMQT[A,TxPortOverAMQPTwistedPairXForm] {                
      override def exchange : String = {
	throw new Exception( "use mnkrExchange instead " )
      }
      override def routingKey : String = {
	throw new Exception( "use mnkrRoutingKey instead " )
      }

      override def zero [A] : TxPortOverAMQPTwistedPairXForm[A] = {
	val sHost = mnkrHost( srcMoniker )
	val sPort = mnkrPort( srcMoniker )
	val sExchange = mnkrExchange( srcMoniker )
	val sRoutingKey = mnkrRoutingKey( srcMoniker )

	val tHost = mnkrHost( trgtMoniker )
	val tPort = mnkrPort( trgtMoniker )
	val tExchange = mnkrExchange( trgtMoniker )
	val tRoutingKey = mnkrRoutingKey( trgtMoniker )

	new TxPortOverAMQPTwistedPairXForm[A](
	  TwistedQueuePair[TxPort](
	    AMQPQueue[TxPort](
	      sExchange,
	      sRoutingKey,
	      theMDS.serve[TxPort]( factory, sHost, sPort, sExchange ),
	      theMDS.sender[TxPort]( sHost, sPort, sExchange, sRoutingKey )
	    ),
	    AMQPQueue[TxPort](
	      tExchange,
	      tRoutingKey,
	      theMDS.serve[TxPort]( factory, tHost, tPort, tExchange ),
	      theMDS.sender[TxPort]( tHost, tPort, tExchange, tRoutingKey )
	    )
	  )
	)
      }
    }
  }
  
  override def tap [A] ( fact : A ) : Unit = { BasicLogService.reportage( fact ) }
    
  def srcHost : String = mnkrHost( name )  
  def srcPort : Int = mnkrPort( name )
  def srcExchange : String = mnkrExchange( name )  
  def srcRoutingKey : String = mnkrRoutingKey( name )

  def srcScope( src : Moniker, trgt : Moniker ) : AMQPTxPortFramedMsgScope =
    new AMQPTxPortFramedMsgScope( src, trgt )

  def scopeMap( trgts : Iterable[Moniker] ) : HashMap[Moniker,AMQPTxPortFramedMsgScope] = {
    val sMap = new HashMap[Moniker,AMQPTxPortFramedMsgScope]()
    for( trgt <- trgts ) { sMap += ( trgt -> srcScope( name, trgt ) ) }
    sMap
  }  

  @transient lazy val stblScopeMap : HashMap[Moniker,AMQPTxPortFramedMsgScope] = 
    scopeMap( acquaintances )
  
  def mnkrTPM(
    srcMoniker : Moniker,
    trgtMoniker : Moniker
  ) : Option[AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedQueuePairM[FramedMsg]] = {
    for( scope <- stblScopeMap.get( trgtMoniker ) ) yield {
      new scope.TxPortOverAMQPTwistedQueuePairM[FramedMsg]( srcMoniker, trgtMoniker )
    }
  }
  
  def mkTPM( trgtMoniker : Moniker ) : Option[AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedQueuePairM[FramedMsg]] =
    mnkrTPM( name, trgtMoniker )
  def tpmMap( trgts : Iterable[Moniker] ) : HashMap[Moniker,AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedQueuePairM[FramedMsg]] = {
    val tpmMap = new HashMap[Moniker,AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedQueuePairM[FramedMsg]]()
    for( trgt <- trgts; tpm <- mnkrTPM( name, trgt ) ) { tpmMap += ( trgt -> tpm ) }
    tpmMap
  }
  @transient lazy val stblTPMMap : HashMap[Moniker,AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedQueuePairM[FramedMsg]] = 
    tpmMap( acquaintances )
    
  def mnkrQ( mnkr : Moniker ) : Option[AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedPairXForm[FramedMsg]] = {
    for( tpm <- stblTPMMap.get( mnkr ) ) yield { tpm.zero[FramedMsg] }
  }       
  def qMap( trgts : Iterable[Moniker] ) : HashMap[Moniker,AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedPairXForm[FramedMsg]] = {
    val queueMap = new HashMap[Moniker,AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedPairXForm[FramedMsg]]()
    for( trgt <- trgts ) {
      val tpm =
	stblTPMMap.get( trgt ) match {
	  case Some( tpm ) => tpm
	  case _ => {
	    mkTPM( trgt ) match {
	      case Some( tpm ) => {
		stblTPMMap += ( trgt -> tpm )
		tpm
	      }
	      case _ => {
		val sScope = srcScope( name, trgt )
		stblScopeMap += ( trgt -> sScope )
		val tpm : AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedQueuePairM[FramedMsg] =
		  new sScope.TxPortOverAMQPTwistedQueuePairM[FramedMsg]( name, trgt )
		stblTPMMap += ( trgt -> tpm )
		tpm
	      }
	    }
	  }
	}

      queueMap += ( trgt -> tpm.zero[FramedMsg] )
    }
    queueMap
  }
  @transient lazy val stblQMap : HashMap[Moniker,AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedPairXForm[FramedMsg]] = 
    qMap( acquaintances )

  def !( request : ReqBody ) : Unit = {
    for ( trgt <- acquaintances; q <- stblQMap.get( trgt ) ) {
      q ! frameRequest( trgt )( request )
    }
  }

  def !!( response : RspBody ) : Unit = {
    for ( trgt <- acquaintances; q <- stblQMap.get( trgt ) ) {
      q ! frameResponse( trgt )( response )
    }
  }

  def ?() = {
    Generator {
      k : ( Either[ReqBody,RspBody] => Unit @suspendable ) => {
	val acqItr = acquaintances.iterator
	while( acqItr.hasNext ) {
	  val trgt = acqItr.next.asInstanceOf[Moniker]
	  val ( q, tpm, scope ) =
	    ( stblQMap( trgt ), stblTPMMap( trgt ), stblScopeMap( trgt ) );
	  val tpmp : scope.TxPortOverAMQPTwistedQueuePairM[FramedMsg] =
	    tpm.asInstanceOf[scope.TxPortOverAMQPTwistedQueuePairM[FramedMsg]]
	  val qp : scope.TxPortOverAMQPTwistedPairXForm[FramedMsg] =
	    q.asInstanceOf[scope.TxPortOverAMQPTwistedPairXForm[FramedMsg]]
	  
	  for( msg <- tpmp( qp ) ) {
	    reset{ k( body( msg ) ) }
	  }
	}
      }
    }
  }

  def ??() = {
    Generator {
      k : ( FramedMsg => Unit @suspendable ) => {
	val acqItr = acquaintances.iterator
	while( acqItr.hasNext ) {
	  val trgt = acqItr.next.asInstanceOf[Moniker]
	  BasicLogService.tweet( "getting next message in target queue of " + trgt )
	  val ( q, tpm, scope ) =
	    ( stblQMap( trgt ), stblTPMMap( trgt ), stblScopeMap( trgt ) );
	  val tpmp : scope.TxPortOverAMQPTwistedQueuePairM[FramedMsg] =
	    tpm.asInstanceOf[scope.TxPortOverAMQPTwistedQueuePairM[FramedMsg]]
	  val qp : scope.TxPortOverAMQPTwistedPairXForm[FramedMsg] =
	    q.asInstanceOf[scope.TxPortOverAMQPTwistedPairXForm[FramedMsg]]
	  
	  for( msg <- tpmp( qp ) ) {
	    BasicLogService.tweet( "handling " + msg + " in target queue of " + trgt )
	    reset{ k( msg ) }
	  }
	}
      }
    }
  }
}

class MonadicJSONFramedMsgDispatcher[ReqBody,RspBody](
  override val individuality : Individual[ReqBody,RspBody,MonadicJSONFramedMsgDispatcher],
  override val acquaintances : List[Moniker]
) extends MonadicTxPortFramedMsgDispatcher[String,ReqBody,RspBody,MonadicJSONFramedMsgDispatcher](
  individuality, acquaintances
) with MessageFraming[String,ReqBody,RspBody] {
  import identityConversions._
  
  def txPort2FramedMsg [A <: FramedMsg] ( txPortMsg : String ) : A = {
    //BasicLogService.tweet( "message before xform : " + txPortMsg )
    val xstrm = new XStream( new JettisonMappedXmlDriver )
    val msgA = xstrm.fromXML( txPortMsg ).asInstanceOf[A]
    //BasicLogService.tweet( "message after xform : " + msgA )
    msgA
  }
  def framedMsg2TxPort [A >: FramedMsg] ( txPortMsg : A ) : String = {
    BasicLogService.tweet( "message before xform : " + txPortMsg )
    val xstrm = new XStream( new JettisonMappedXmlDriver )
    val msgStr = xstrm.toXML( txPortMsg )
    BasicLogService.tweet( "message after xform : " + msgStr )
    msgStr
  }
}

object MonadicJSONFramedMsgDispatcher {
  def apply [ReqBody,RspBody] (
    individuality : Individual[ReqBody,RspBody,MonadicJSONFramedMsgDispatcher],
    acquaintances : List[Moniker]
  ) : MonadicJSONFramedMsgDispatcher[ReqBody,RspBody] = {
    new MonadicJSONFramedMsgDispatcher[ReqBody,RspBody](
      individuality, acquaintances
    )
  }
  def unapply [ReqBody,RspBody] (
    dispatcher : MonadicJSONFramedMsgDispatcher[ReqBody,RspBody]
  ) : Option[(Individual[ReqBody,RspBody,MonadicJSONFramedMsgDispatcher],List[Moniker])]
  = {
    Some(
      (
	dispatcher.individuality,
	dispatcher.acquaintances
      )
    )
  }
}

package usage {
  import com.biosimilarity.lift.lib.amqp.utilities._
  object MsgStreamFactory extends AMQPTestUtility[String] {
    override def msgStreamPayload( idx : Int ) : String = { "Msg" + idx }  
  }
  object FramedMsgDispatcherUseCase {
    trait UseCaseProtocol extends MsgStreamFactory.Message
    trait UseCaseRequest extends UseCaseProtocol
    trait UseCaseResponse extends UseCaseProtocol
    case class UseCaseRequestOne(
      b : Boolean, i : Int, a : String, r : Option[MsgStreamFactory.Message]
    ) extends UseCaseRequest 
    case class UseCaseResponseOne(
      b : Boolean, i : Int, a : String, r : Option[MsgStreamFactory.Message]
    ) extends UseCaseResponse
    case class FramedUseCaseProtocolDispatcher(
      here : URI, there : URI
    ) extends MonadicJSONFramedMsgDispatcher[UseCaseRequest,UseCaseResponse](
      Individual(
	MURI( here ),
	new ListBuffer[JustifiedRequest[UseCaseRequest,UseCaseResponse]](),
	new ListBuffer[JustifiedResponse[UseCaseRequest,UseCaseResponse]]()
      ),
      List[Moniker]( MURI( there ) )
    ) {      
      override def toString() : String = {
	"FramedUseCaseProtocolDispatcher" + "[" + here + " -> " + there + "]"
      }
    }
    implicit val retTwist : Boolean = false
    def setup(
      localHost : String, localPort : Int,
      remoteHost : String, remotePort : Int
    )( implicit returnTwist : Boolean ) : Either[FramedUseCaseProtocolDispatcher,(FramedUseCaseProtocolDispatcher,FramedUseCaseProtocolDispatcher)] = {
      val ( localExchange, remoteExchange ) = 
	if ( localHost.equals( remoteHost ) && ( localPort == remotePort ) ) {
	  ( "/useCaseProtocolLocal", "/useCaseProtocolRemote" )	  
	}
	else {
	  ( "/useCaseProtocol", "/useCaseProtocol" )	  
	}

      if ( returnTwist ) {
	Right[FramedUseCaseProtocolDispatcher,(FramedUseCaseProtocolDispatcher,FramedUseCaseProtocolDispatcher)](
	  (
	    FramedUseCaseProtocolDispatcher(
	      new URI( "agent", null, localHost, localPort, localExchange, null, null ),
	      new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
	    ),
	    FramedUseCaseProtocolDispatcher(	      
	      new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null ),
	      new URI( "agent", null, localHost, localPort, localExchange, null, null )
	    )
	  )
	)
      }
      else {
	Left[FramedUseCaseProtocolDispatcher,(FramedUseCaseProtocolDispatcher,FramedUseCaseProtocolDispatcher)](
	  FramedUseCaseProtocolDispatcher(
	    new URI( "agent", null, localHost, localPort, localExchange, null, null ),
	    new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
	  )
	)
      }
    }

    implicit val numberOfMsgs : Int = 100
    def runClient( dispatcher : FramedUseCaseProtocolDispatcher )( implicit numMsgs : Int ) : Unit = {
      val reqs = MsgStreamFactory.msgStream[UseCaseRequest](
	( b : Boolean, i : Int, a : String, r : Option[MsgStreamFactory.Message] ) => {
	  UseCaseRequestOne( b, i, a, r )
	}
      ).take( numMsgs ).toList      
      
      // map-reduce-style protocol checking

      val msgMap =
	new HashMap[Int,Either[UseCaseRequest,(UseCaseRequest,UseCaseResponse)]]()

      msgMap += ( 0 -> Left[UseCaseRequest,(UseCaseRequest,UseCaseResponse)]( reqs( 0 ) ) )
      dispatcher ! reqs( 0 )      

      new Thread {
	override def run() : Unit = {
	  reset {
	    for( msg <- dispatcher ?() ) {
	      BasicLogService.tweet( dispatcher + " received: " + msg )
	      msg match {
		case Right( rsp@UseCaseResponseOne( b, j, a, r ) ) => {
		  BasicLogService.tweet( dispatcher + " handling response for the " + j + "th " + "request" )
		  msgMap.get( j ) match {		
		    case Some( Left( req ) ) => {
		      msgMap += ( j -> Right[UseCaseRequest,(UseCaseRequest,UseCaseResponse)]( ( req, rsp ) ) )
		      if ( j < ( numMsgs - 1 ) ) {
			// Open with left brace ...
			msgMap += ( ( j + 1 ) -> Left[UseCaseRequest,(UseCaseRequest,UseCaseResponse)]( reqs( j + 1 ) ) )
			dispatcher ! reqs( j + 1 ) 
		      }
		      else {
			BasicLogService.tweet( "Client side of test complete." )
		      }
		    }
		    case unExpected@_ => {
		      throw new Exception( "Protocol violated. Received: " + unExpected )
		    }
		  }
		}
		case unExpected@_ => {
		  throw new Exception( "Protocol violated. Received: " + unExpected )
		}	    
	      }
	    }
	  }
	}
      }.start
    }

    def runServer( dispatcher : FramedUseCaseProtocolDispatcher )( implicit numMsgs : Int ) : Unit = {                  
      // map-reduce-style protocol checking
      val msgMap =
	new HashMap[Int,Either[UseCaseRequest,(UseCaseRequest,UseCaseResponse)]]()

      new Thread {
	override def run() : Unit = {
	  reset {
	    for( msg <- dispatcher ?() ) {
	      BasicLogService.tweet( dispatcher + " received: " + msg )
	      msg match {
		// Close with right brace ...
		case Left( req@UseCaseRequestOne( b, j, a, r ) ) => {
		  BasicLogService.tweet( dispatcher + " handling the " + j + "th " + "request" )
		  if ( j < numMsgs ) {
		    msgMap.get( j ) match {
		      case None => {
			val rsp = UseCaseResponseOne( b, j, a, Some( req ) )
			msgMap +=
			( j -> Right[UseCaseRequest,(UseCaseRequest,UseCaseResponse)]( req, rsp ) )	
			dispatcher !! rsp
			if ( msgMap.size == numMsgs ) {
			  BasicLogService.tweet( "Server side of test complete." )
			}
		      }
		      case unExpected@_ => {
			throw new Exception( "Protocol violated. Received: " + unExpected )
		      }
		    }		
		  }
		  else {
		    BasicLogService.tweet( "Server side of test complete." )
		  }	      
		}
		case unExpected@_ => {
		  throw new Exception( "Protocol violated. Received: " + unExpected )
		}
	      }
	    }
	  }
	}
      }.start
      
    }
  }
}
