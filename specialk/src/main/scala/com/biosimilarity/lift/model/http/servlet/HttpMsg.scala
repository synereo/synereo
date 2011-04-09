// -*- mode: Scala;-*- 
// Filename:    HttpMsg.scala 
// Authors:     lgm                                                    
// Creation:    Wed Aug 25 12:17:49 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.http.servlet

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._

import java.net.URI
import java.util.UUID

import javax.servlet._
import javax.servlet.http.HttpUtils
import javax.servlet.http.{HttpServlet,
  HttpServletRequest => HSReq, HttpServletResponse => HSResp}

class JustHttpRequest(
  override val msgId  : UUID,
  override val to     : URI,
  override val from   : URI,
  override val flowId : UUID,
  override val body   : HSReq,
  override val justification : Option[Response[AbstractJustifiedRequest[HSReq,HSResp],HSResp]]
) extends AbstractJustifiedRequest[HSReq,HSResp](
  msgId, to, from, flowId, body, justification
)


