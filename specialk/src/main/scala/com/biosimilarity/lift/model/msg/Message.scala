// -*- mode: Scala;-*- 
// Filename:    TheMessage.scala 
// Authors:     lgm                                                    
// Creation:    Fri Oct 16 14:18:12 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.msg

import com.biosimilarity.lift.lib._

import java.net.URI
import java.util.UUID

trait Header {
  def msgId         : UUID
  def to            : URI
  def from          : URI
  def flowId        : UUID
}

trait Message[Justfication,BodyType] {  
  def body          : BodyType
  def justification : Option[Justfication]  
}

abstract class Request[Response,BodyType](
  override val msgId  : UUID,
  override val to     : URI,
  override val from   : URI,
  override val flowId : UUID,
  override val body   : BodyType,
  override val justification : Option[Response]  
) extends Message[Response,BodyType] with Header

class AbstractJustifiedRequest[ReqBody,RspBody](
  override val msgId  : UUID,
  override val to     : URI,
  override val from   : URI,
  override val flowId : UUID,
  override val body   : ReqBody,
  override val justification : Option[Response[AbstractJustifiedRequest[ReqBody,RspBody],RspBody]]
) extends Request[Response[AbstractJustifiedRequest[ReqBody,RspBody],RspBody],ReqBody](
  msgId, to, from, flowId, body, justification
)

object AbstractJustifiedRequest {
  def apply[ReqBody,RspBody](
    msgId         : UUID,
    to            : URI,
    from          : URI,
    flowId        : UUID,
    body          : ReqBody,
    justification : Option[Response[AbstractJustifiedRequest[ReqBody,RspBody],RspBody]]
  )
  : AbstractJustifiedRequest[ReqBody,RspBody] = {
    new AbstractJustifiedRequest(
      msgId, to, from, flowId, body, justification
    )
  }  
  def unapply[ReqBody,RspBody]( ajr : AbstractJustifiedRequest[ReqBody,RspBody] )
  : Option[(UUID,URI,URI,UUID,ReqBody,Option[Response[AbstractJustifiedRequest[ReqBody,RspBody],RspBody]])]
  = {
    Some(( ajr.msgId, ajr.to, ajr.from, ajr.flowId, ajr.body, ajr.justification ))
  }    
}

case class JustifiedRequest[ReqBody,RspBody](
  override val msgId  : UUID,
  override val to     : URI,
  override val from   : URI,
  override val flowId : UUID,
  override val body   : ReqBody,
  override val justification : Option[Response[AbstractJustifiedRequest[ReqBody,RspBody],RspBody]]
) extends AbstractJustifiedRequest[ReqBody,RspBody](
  msgId, to, from, flowId, body, justification
)

case class URIJustifiedRequest[ReqBody](
  override val msgId  : UUID,
  override val to     : URI,
  override val from   : URI,
  override val flowId : UUID,
  override val body   : ReqBody,
  override val justification : Option[Response[AbstractJustifiedRequest[ReqBody,URI],URI]]
) extends AbstractJustifiedRequest[ReqBody,URI](
  msgId, to, from, flowId, body, justification
)

abstract class Response[Request,BodyType](
  override val msgId  : UUID,
  override val to     : URI,
  override val from   : URI,
  override val flowId : UUID,
  override val body   : BodyType,
  override val justification : Option[Request]
) extends Message[Request,BodyType] with Header

class AbstractJustifiedResponse[ReqBody,RspBody](
  override val msgId  : UUID,
  override val to     : URI,
  override val from   : URI,
  override val flowId : UUID,
  override val body   : RspBody,
  override val justification : Option[Request[AbstractJustifiedResponse[ReqBody,RspBody],ReqBody]]
) extends Response[Request[AbstractJustifiedResponse[ReqBody,RspBody],ReqBody],RspBody](
  msgId, to, from, flowId, body, justification
)

object AbstractJustifiedResponse {
  def apply[ReqBody,RspBody](
    msgId         : UUID,
    to            : URI,
    from          : URI,
    flowId        : UUID,
    body          : RspBody,
    justification : Option[Request[AbstractJustifiedResponse[ReqBody,RspBody],ReqBody]]
  ) : AbstractJustifiedResponse[ReqBody,RspBody] = {
    new AbstractJustifiedResponse(
      msgId, to, from, flowId, body, justification
    )
  }
  def unapply[ReqBody,RspBody]( ajr : AbstractJustifiedResponse[ReqBody,RspBody] )
  : Option[(UUID,URI,URI,UUID,RspBody,Option[Request[AbstractJustifiedResponse[ReqBody,RspBody],ReqBody]])]
  = {
      Some( ( ajr.msgId, ajr.to, ajr.from, ajr.flowId, ajr.body, ajr.justification ) )
  }  
}

case class JustifiedResponse[ReqBody,RspBody](
  override val msgId  : UUID,
  override val to     : URI,
  override val from   : URI,
  override val flowId : UUID,
  override val body   : RspBody,
  override val justification : Option[Request[AbstractJustifiedResponse[ReqBody,RspBody],ReqBody]]
) extends AbstractJustifiedResponse[ReqBody,RspBody](
  msgId, to, from, flowId, body, justification
)

case class URIJustifiedResponse[RspBody](
  override val msgId  : UUID,
  override val to     : URI,
  override val from   : URI,
  override val flowId : UUID,
  override val body   : RspBody,
  override val justification : Option[Request[AbstractJustifiedResponse[URI,RspBody],URI]]
) extends AbstractJustifiedResponse[URI,RspBody](
  msgId, to, from, flowId, body, justification
)

// Inspection and other control plane messages

trait InspectionRequest
     extends Message[Unit,Unit]
     with Header
     with UUIDOps {
  val flowId : UUID = getUUID()
}

case class InspectRequests( to : URI, from : URI )
     extends InspectionRequest {
       override def msgId = getUUID()
       override def body = {}
       override def justification = None       
     }

case class InspectResponses( to : URI, from : URI )
     extends InspectionRequest {
       override def msgId = getUUID()
       override def body = {}
       override def justification = None
     }

case class InspectNamespace( to : URI, from : URI )
     extends InspectionRequest {
       override def msgId = getUUID()
       override def body = {}
       override def justification = None
     }

trait InspectionResponse[Request] extends Message[Request,Unit] {}
