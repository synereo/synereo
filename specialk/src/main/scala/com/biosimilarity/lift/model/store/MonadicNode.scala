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

abstract class MonadicTxPortFramedMsgDispatcher[TxPort,ReqBody,RspBody](
  override val name : Moniker,
  override val requests : ListBuffer[JustifiedRequest[ReqBody,RspBody]],
  override val responses : ListBuffer[JustifiedResponse[ReqBody,RspBody]],
  @transient val nameMap : Option[LinkedHashMap[Moniker,MonadicTxPortFramedMsgDispatcher[TxPort,ReqBody,RspBody]]],
  @transient override val traceMonitor : TraceMonitor[ReqBody,RspBody]
) extends Socialite[ReqBody,RspBody]
  with Awareness[ReqBody,RspBody]
  with Focus[ReqBody,RspBody] 
  with MonadicGenerators
  with MonadicConcurrentGenerators
  with AMQPMonikerOps
  with FJTaskRunners
  with WireTap 
  with Journalist
{  
  import identityConversions._
  override def nameSpace : Option[LinkedHashMap[Moniker,Socialite[ReqBody,RspBody]]] =
    nameMap.asInstanceOf[Option[LinkedHashMap[Moniker,Socialite[ReqBody,RspBody]]]]

  type FramedMsg = Either[JustifiedRequest[ReqBody,RspBody],JustifiedResponse[ReqBody,RspBody]]
  def txPort2FramedMsg [A <: FramedMsg] ( txPortMsg : TxPort ) : A
  def framedMsg2TxPort [A >: FramedMsg] ( txPortMsg : A ) : TxPort

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
	txPort2FramedMsg[FramedMsg]( txPortMsg ) match {
	  case fmsg : FMsg => fmsg
	  case _ => {
	    throw new Exception( "trgt is not an FMsg" )
	  }
	}
      },
      ( fmsg : FMsg ) => {
	fmsg match {
	  case trgt : FramedMsg => {
	    framedMsg2TxPort[FramedMsg]( fmsg.asInstanceOf[FramedMsg] )
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

  override def useBraceNotation : Boolean = false  
  def likes( dsg : Moniker, acq : Socialite[ReqBody,RspBody] ) : Boolean = true
       
  override def handleRequestPayload ( payload : ReqBody ) : Boolean = false
  override def handleResponsePayload ( payload : RspBody ) : Boolean = false  

  override def tap [A] ( fact : A ) : Unit = { reportage( fact ) }
    
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

  @transient lazy val stblScopeMap : Option[HashMap[Moniker,AMQPTxPortFramedMsgScope]] = 
    for( ns <- nameSpace ) yield { scopeMap( ns.keys ) }
  
  def mnkrTPM(
    srcMoniker : Moniker,
    trgtMoniker : Moniker
  ) : Option[AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedQueuePairM[FramedMsg]] = {
    for( ssMap <- stblScopeMap; scope <- ssMap.get( trgtMoniker ) ) yield {
      new scope.TxPortOverAMQPTwistedQueuePairM( srcMoniker, trgtMoniker )
    }
  }
  
  def mkTPM( trgtMoniker : Moniker ) : Option[AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedQueuePairM[FramedMsg]] =
    mnkrTPM( name, trgtMoniker )
  def tpmMap( trgts : Iterable[Moniker] ) : HashMap[Moniker,AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedQueuePairM[FramedMsg]] = {
    val tpmMap = new HashMap[Moniker,AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedQueuePairM[FramedMsg]]()
    for( trgt <- trgts; tpm <- mnkrTPM( name, trgt ) ) { tpmMap += ( trgt -> tpm ) }
    tpmMap
  }
  @transient lazy val stblTPMMap : Option[HashMap[Moniker,AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedQueuePairM[FramedMsg]]] = 
    for( ns <- nameSpace ) yield { tpmMap( ns.keys ) }
    
  def mnkrQ( mnkr : Moniker ) : Option[AMQPTxPortFramedMsgScope#TxPortOverAMQPTwistedPairXForm[FramedMsg]] = {
    for( stpmMap <- stblTPMMap; tpm <- stpmMap.get( mnkr ) ) yield { tpm.zero }
  }       
}

class MonadicJSONFramedMsgDispatcher[ReqBody,RspBody](
  override val name : Moniker,
  override val requests : ListBuffer[JustifiedRequest[ReqBody,RspBody]],
  override val responses : ListBuffer[JustifiedResponse[ReqBody,RspBody]],
  @transient override val nameMap : Option[LinkedHashMap[Moniker,MonadicTxPortFramedMsgDispatcher[String,ReqBody,RspBody]]],
  @transient override val traceMonitor : TraceMonitor[ReqBody,RspBody]
) extends MonadicTxPortFramedMsgDispatcher[String,ReqBody,RspBody](
  name, requests, responses, nameMap, traceMonitor
) {
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

object MonadicJSONFramedMsgDispatcher {
  def apply [ReqBody,RspBody] (
    name : Moniker,
    requests : ListBuffer[JustifiedRequest[ReqBody,RspBody]],
    responses : ListBuffer[JustifiedResponse[ReqBody,RspBody]],
    nameMap : Option[LinkedHashMap[Moniker,MonadicTxPortFramedMsgDispatcher[String,ReqBody,RspBody]]],
    traceMonitor : TraceMonitor[ReqBody,RspBody]
  ) : MonadicJSONFramedMsgDispatcher[ReqBody,RspBody] = {
    new MonadicJSONFramedMsgDispatcher[ReqBody,RspBody](
      name, requests, responses, nameMap, traceMonitor
    )
  }
  def unapply [ReqBody,RspBody] (
    dispatcher : MonadicJSONFramedMsgDispatcher[ReqBody,RspBody]
  ) : Option[(Moniker,ListBuffer[JustifiedRequest[ReqBody,RspBody]],ListBuffer[JustifiedResponse[ReqBody,RspBody]],Option[LinkedHashMap[Moniker,MonadicTxPortFramedMsgDispatcher[String,ReqBody,RspBody]]],TraceMonitor[ReqBody,RspBody])]
  = {
    Some(
      (
	dispatcher.name,
	dispatcher.requests,
	dispatcher.responses,
	dispatcher.nameMap,
	dispatcher.traceMonitor
      )
    )
  }
}
