// -*- mode: Scala;-*- 
// Filename:    SpecialKMessenger.scala 
// Authors:     lgm                                                    
// Creation:    Wed Aug 25 13:58:10 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._

import net.liftweb.amqp._

import scala.collection.mutable._

import java.net.URI
import java.util.UUID

trait AgentsOverAMQP {
  type JSONOverAMQP = AMQPMessage[String]
  type JTSReq = JustifiedRequest[JSONOverAMQP,JSONOverAMQP]
  type JTSRsp = JustifiedResponse[JSONOverAMQP,JSONOverAMQP]

  object IdVendor extends UUIDOps {
    def getURI() = {
      new URI( "com", "biosimilarity", getUUID().toString )
    }
  }

  object AnAMQPTraceMonitor extends TraceMonitor[JSONOverAMQP,JSONOverAMQP]  

  class AMQPAgent(
    alias : URI
  ) extends ReflectiveMessenger[JSONOverAMQP,JSONOverAMQP](
    alias,
    new ListBuffer[JTSReq](),
    new ListBuffer[JTSRsp](),
    Some( new LinkedHashMap[URI,Socialite[JSONOverAMQP,JSONOverAMQP]]),
    AnAMQPTraceMonitor
  )
  with UUIDOps {
    override def validateTarget( msg : {def to : URI} ) : Boolean = {
      // Put URI filtering behavior here
      true
    }
    
    override def validateAcquaintance( msg : {def from : URI} ) : Boolean = {
      // Put Requestor filtering behavior here
      nameSpace match {
	case None => false
	case Some( map ) => true
      }
    }

    override def handleWithContinuation(
      request : JTSReq,
      k : Status[JTSReq] => Status[JTSReq]
    ) = {
      //println( "handling: " + request )
      request match {
	case JustifiedRequest(
	  msgId, trgt, src, lbl, body, None
	) => { 
	  // Handle a justified request with no initiating response
	}
	case _ => {
	  // Handle a justified request with an initiating response
	}
      }
      JReqStatus(
	request,
	true,
	None,
	Some( k )
      )
    }  
  }
  
}
