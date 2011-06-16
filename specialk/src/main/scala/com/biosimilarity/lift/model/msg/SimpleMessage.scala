// -*- mode: Scala;-*- 
// Filename:    SimpleMessage.scala 
// Authors:     lgm                                                    
// Creation:    Tue Nov 10 19:51:10 2009 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.msg

import com.biosimilarity.lift.lib.moniker._

import java.net.URI
import java.util.UUID

case class JustStrRequest(
  override val msgId  : UUID,
  //override val to     : URI,
  override val to     : Moniker,
  //override val from   : URI,
  override val from   : Moniker,
  override val flowId : UUID,
  override val body   : String,
  override val justification : Option[Response[AbstractJustifiedRequest[String,String],String]]
) extends AbstractJustifiedRequest[String,String](
  msgId, to, from, flowId, body, justification
)

case class JustStrResponse(
  override val msgId  : UUID,
  //override val to     : URI,
  override val to     : Moniker,
  //override val from   : URI,
  override val from   : Moniker,
  override val flowId : UUID,
  override val body   : String,
  override val justification : Option[Request[AbstractJustifiedResponse[String,String],String]]
) extends AbstractJustifiedResponse[String,String](
  msgId, to, from, flowId, body, justification
)

// case class JustCnxnMgrMsg(
//   msgId         : UUID,
//   to            : URI,
//   from          : URI,
//   flowId        : UUID,
//   body          : ConnectionManagerMsg,
//   justification : Option[Request[JustifiedResponse[ConnectionManagerMsg,ConnectionManagerMsg],ConnectionManagerMsg]]
// ) extends JustifiedRequest[ConnectionManagerMsg,ConnectionManagerMsg](
//   msgId, to, from, flowId, body, justification
// )
