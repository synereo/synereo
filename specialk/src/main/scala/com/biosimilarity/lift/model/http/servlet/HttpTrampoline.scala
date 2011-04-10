// -*- mode: Scala;-*- 
// Filename:    HttpTrampoline.scala 
// Authors:     lgm                                                    
// Creation:    Sat Apr  9 04:09:35 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.http.servlet

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._

import scala.util.continuations._
import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.collection.mutable._

import java.net.URI
import java.util.UUID

import javax.servlet._
import javax.servlet.{ServletRequest => HSReq, ServletResponse => HSRsp}
import javax.servlet.http.HttpUtils
import javax.servlet.http.{HttpServlet}
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

case class HTTPRequestCtxt(
  req : HttpServletRequest,
  resp : HttpServletResponse,
  chain : FilterChain
)

trait HTTPTrampoline[Namespace,Var,Tag] {
  self : DTSMsgScope[Namespace,Var,Tag,HTTPRequestCtxt] => 
  trait HTTPToCnxnCtxtLabel[Namespace,Var,Tag] {  
    def asCall(
      hctxt : HTTPRequestCtxt
    ) : Option[Msgs.MDistributedTermSpaceRequest[Namespace,Var,Tag,HTTPRequestCtxt]]
    def rspPickupLoc(
      hctxt : HTTPRequestCtxt
    ) : Option[Msgs.MDistributedTermSpaceRequest[Namespace,Var,Tag,HTTPRequestCtxt]]
    def asCCL(
      hctxt : HTTPRequestCtxt
    ) : Option[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
    def asCCL(
      hsrq : HttpServletRequest
    ) : Option[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
    def asCCL(
      hsrq : HttpServletResponse
    ) : Option[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
  }  

  type HTTPConverter <: HTTPToCnxnCtxtLabel[Namespace,Var,Tag]

  def protoHTTPConverter : HTTPConverter
  lazy val httpConverter = protoHTTPConverter  
}

trait HTTPTrampolineScope[Namespace,Var,Tag] {
  type HTTPTrampolineTypes <:
  HTTPTrampoline[Namespace,Var,Tag]
     with DTSMsgScope[Namespace,Var,Tag,HTTPRequestCtxt]

  def protoHttpTramp : HTTPTrampolineTypes
  lazy val httpTramp : HTTPTrampolineTypes =
    protoHttpTramp
}

