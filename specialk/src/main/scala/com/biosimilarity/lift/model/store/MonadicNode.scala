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

class MonadicFramedMsgDispatcher[TxPort,ReqBody,RspBody](
  override val name : Moniker,
  override val requests : ListBuffer[JustifiedRequest[ReqBody,RspBody]],
  override val responses : ListBuffer[JustifiedResponse[ReqBody,RspBody]],
  @transient override val nameSpace : Option[LinkedHashMap[Moniker,Socialite[ReqBody,RspBody]]],
  @transient override val traceMonitor : TraceMonitor[ReqBody,RspBody]
) extends Socialite[ReqBody,RspBody]
  with Awareness[ReqBody,RspBody]
  with Focus[ReqBody,RspBody] 
  with MonadicWireToTrgtConversion
  with MonadicGenerators
  with WireToTrgtConversion
  with WireTap 
  with Journalist
{  
  override type Wire = TxPort
  override type Trgt = Either[JustifiedRequest[ReqBody,RspBody],JustifiedResponse[ReqBody,RspBody]]

  override def useBraceNotation : Boolean = false  
  def likes( dsg : Moniker, acq : Socialite[ReqBody,RspBody] ) : Boolean = true
       
  override def handleRequestPayload ( payload : ReqBody ) : Boolean = false
  override def handleResponsePayload ( payload : RspBody ) : Boolean = false  

  override def tap [A] ( fact : A ) : Unit = { reportage( fact ) }
  override def wire2Trgt( wire : Wire ) : Trgt = {
    throw new Exception( "wire2Trgt left unimplemented" )
  }
  override def trgt2Wire( trgt : Trgt ) : Wire = { 
    throw new Exception( "trgt2Wire left unimplemented" )
  }
  
  def srcHost( src : Moniker ) : String = src.getHost
  def srcHost : String = srcHost( name )

  def srcExchange( src : Moniker ) : String =
    src.getPath.split( "/" )( 1 )
  def srcExchange : String = srcExchange( name )

  def srcScope : AMQPScope[TxPort] = new AMQPStdScope[TxPort]()
  @transient lazy val stblSrcScope : AMQPScope[TxPort] = srcScope

  def srcQM( srcHost : String, srcExchange : String ) : stblSrcScope.AMQPQueueHostExchangeM[TxPort] =
    new stblSrcScope.AMQPQueueHostExchangeM( srcHost, srcExchange )
  def srcQM : stblSrcScope.AMQPQueueHostExchangeM[TxPort] = srcQM( srcHost, srcExchange )
  @transient lazy val stblSrcQM : stblSrcScope.AMQPQueueHostExchangeM[TxPort] = srcQM

  def srcQ( srcHost : String, srcExchange : String ) : stblSrcScope.AMQPQueue[String] = 
    srcQM( srcHost, srcExchange ).zero
  def srcQ : stblSrcScope.AMQPQueue[String] = stblSrcQM.zero
}


