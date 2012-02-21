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

abstract class MonadicFramedMsgDispatcher[TxPort,ReqBody,RspBody](
  override val name : Moniker,
  override val requests : ListBuffer[JustifiedRequest[ReqBody,RspBody]],
  override val responses : ListBuffer[JustifiedResponse[ReqBody,RspBody]],
  @transient override val nameSpace : Option[LinkedHashMap[Moniker,Socialite[ReqBody,RspBody]]],
  @transient override val traceMonitor : TraceMonitor[ReqBody,RspBody]
) extends Socialite[ReqBody,RspBody]
  with Awareness[ReqBody,RspBody]
  with Focus[ReqBody,RspBody] 
  with MonadicGenerators
  with MonadicConcurrentGenerators
  with FJTaskRunners
  with WireTap 
  with Journalist
{  
  type Trgt = Either[JustifiedRequest[ReqBody,RspBody],JustifiedResponse[ReqBody,RspBody]]
  def txPort2Trgt [A <: Trgt] ( txPortMsg : TxPort ) : A
  def trgt2TxPort [A >: Trgt] ( txPortMsg : A ) : TxPort

  case class AMQPTxPortTrgtScope(
  ) extends AMQPScope[TxPort](
    AMQPDefaults.defaultConnectionFactory
  ) {
    case class AMQPQueueTxPort2TrgtXForm[T](
      override val exchange : String,
      override val routingKey : String,    
      override val w2T : TxPort => T,
      override val t2W : T => TxPort,
      @transient override val dispatcherW : theMDS.Generator[TxPort,Unit,Unit],
      @transient override val senderW : theMDS.Generator[Unit,TxPort,Unit]
    ) extends AMQPQueueXForm[TxPort,T]
    
    class TxPortOverAMQPQueueXFormM[A <: Trgt](
      val host : String,
      val port : Int,
      override val exchange : String,
      override val routingKey : String
    ) extends AMQPQueueMQT[A,AMQPQueueTxPort2TrgtXForm] {
      override def zero [B] : AMQPQueueTxPort2TrgtXForm[B] = {
	AMQPQueueTxPort2TrgtXForm[B](
	  exchange,
	  routingKey,	  
	  ( txPortMsg : TxPort ) => {
	    txPort2Trgt[Trgt]( txPortMsg ) match {
	      case b : B => b
	      case _ => {
		throw new Exception( "trgt is not a B" )
	      }
	    }
	  },
	  ( b : B ) => {
	    b match {
	      case trgt : Trgt => {
		trgt2TxPort[Trgt]( b.asInstanceOf[Trgt] )
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
      def zeroTrgt [B >: Trgt] : AMQPQueueTxPort2TrgtXForm[B] = {
	AMQPQueueTxPort2TrgtXForm[B](
	  exchange,
	  routingKey,	  
	  txPort2Trgt[A],
	  trgt2TxPort[B],
	  theMDS.serve[TxPort]( factory, host, port, exchange ),
	  theMDS.sender[TxPort]( host, port, exchange, routingKey )
	)
      }    
    }
  }

  override def useBraceNotation : Boolean = false  
  def likes( dsg : Moniker, acq : Socialite[ReqBody,RspBody] ) : Boolean = true
       
  override def handleRequestPayload ( payload : ReqBody ) : Boolean = false
  override def handleResponsePayload ( payload : RspBody ) : Boolean = false  

  override def tap [A] ( fact : A ) : Unit = { reportage( fact ) }
  
  def srcHost( src : Moniker ) : String = src.getHost
  def srcHost : String = srcHost( name )

  def srcPort( src : Moniker ) : Int = src.getPort
  def srcPort : Int = srcPort( name )

  def srcExchange( src : Moniker ) : String = {
    val spath = src.getPath.split( "/" )
    spath.length match {
      case 0 => AMQPDefaults.defaultExchange
      case 1 => AMQPDefaults.defaultExchange
      case 2 => spath( 1 )
    }
  }
  def srcExchange : String = srcExchange( name )  

  def srcRoutingKey( src : Moniker ) : String = {
    val rkA = src.getQuery.split( "," ).filter( ( p : String ) => p.contains( "routingKey" ) )
    rkA.length match {
      case 0 => AMQPDefaults.defaultRoutingKey
      case _ => rkA( 0 ).split( "=" )( 1 )
    }
  }
  def srcRoutingKey : String = srcRoutingKey( name )

  def srcScope : AMQPTxPortTrgtScope = new AMQPTxPortTrgtScope()

  @transient lazy val stblSrcScope : AMQPTxPortTrgtScope = srcScope

  def srcQM(
    srcHost : String,
    srcPort : Int,
    srcExchange : String,
    srcRoutingKey : String
  ) : stblSrcScope.TxPortOverAMQPQueueXFormM[Trgt] = {
    new stblSrcScope.TxPortOverAMQPQueueXFormM(
      srcHost, srcPort, srcExchange, srcRoutingKey
    )
  }
  def srcQM(
    srcMoniker : Moniker
  ) : stblSrcScope.TxPortOverAMQPQueueXFormM[Trgt] = {
    new stblSrcScope.TxPortOverAMQPQueueXFormM(
      srcHost( srcMoniker ),
      srcPort( srcMoniker ),
      srcExchange( srcMoniker ),
      srcRoutingKey( srcMoniker )
    )
  }
  
  implicit def srcQM : stblSrcScope.TxPortOverAMQPQueueXFormM[Trgt] = srcQM( name )
  @transient lazy val stblSrcQM : stblSrcScope.TxPortOverAMQPQueueXFormM[Trgt] = srcQM

  def srcQ(
    srcHost : String,
    srcPort : Int,
    srcExchange : String,
    srcRoutingKey : String
  ) : stblSrcScope.AMQPQueueTxPort2TrgtXForm[Trgt] = {
    srcQM( srcHost, srcPort, srcExchange, srcRoutingKey ).zeroTrgt
  }
  implicit def srcQ : stblSrcScope.AMQPQueueTxPort2TrgtXForm[Trgt] = stblSrcQM.zeroTrgt

  // def dispatch(
//     implicit
//     queueMnd : stblSrcScope.AMQPQueueM[String],
//     queue : stblSrcScope.AMQPQueue[String] )(
//   ) : Generator[Trgt,Unit,Unit] = {
//     Generator {
//       k : ( Trgt => Unit @suspendable ) =>
// 	shift {
// 	  outerK : ( Unit => Unit ) =>
// 	    reset {
// 	      for( msg <- queueMnd( queue ) ) {
// 		msg match {
// 		  case l@Left( jreq : JustifiedRequest ) => {
// 		    if ( validate( jreq ) ) {
// 		      reportage( "calling handler on " + jreq )
// 		      k( l )
// 		    }
// 		  }
// 		  case r@Right( jrsp : JustifiedResponse ) => {
// 		    if ( validate( jrsp ) ) {
// 		      reportage( "calling handler on " + jrsp )
// 		      k( r )
// 		    }
// 		  }
// 		}
// 	      }
// 	    }
// 	}
//     }
      
}


