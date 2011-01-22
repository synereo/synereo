// -*- mode: Scala;-*- 
// Filename:    DistribTermSpaceMsg.scala 
// Authors:     lgm                                                    
// Creation:    Fri Jan 21 20:32:18 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._

import scala.collection.mutable._
import scala.util.continuations._ 

import java.net.URI
import java.util.UUID

trait DistributedTermSpaceMsg[Namespace,Var,Tag,Value]
trait DistributedTermSpaceRequest[Namespace,Var,Tag,Value]
extends DistributedTermSpaceMsg[Namespace,Var,Tag,Value]
trait DistributedTermSpaceResponse[Namespace,Var,Tag,Value]
extends DistributedTermSpaceMsg[Namespace,Var,Tag,Value]

// trait DistributedSelfAssemblyMsg
// trait DistributedSelfAssemblyRequest
// extends DistributedSelfAssemblyMsg
// trait DistributedSelfAssemblyResponse
// extends DistributedSelfAssemblyMsg

// case class DConnect( src : URI, trgt : URI, kuid : UUID ) 
// extends DistributedSelfAssemblyRequest
// with DistributedTermSpaceRequest
// case class DConnected( src : URI, trgt : URI, kuid : UUID ) 
// extends DistributedSelfAssemblyResponse
// with DistributedTermSpaceResponse
// case class DConnectionRefused( src : URI, trgt : URI, kuid : UUID ) 
// extends DistributedSelfAssemblyResponse
// with DistributedTermSpaceResponse

case class DGetRequest[Namespace,Var,Tag,Value](
  path : CnxnCtxtLabel[Namespace,Var,Tag]
) extends DistributedTermSpaceRequest[Namespace,Var,Tag,Value]
case class DGetResponse[Namespace,Var,Tag,Value](
  path : CnxnCtxtLabel[Namespace,Var,Tag],
  value : Value
) extends DistributedTermSpaceResponse[Namespace,Var,Tag,Value]
case class DFetchRequest[Namespace,Var,Tag,Value](
  path : CnxnCtxtLabel[Namespace,Var,Tag]
) extends DistributedTermSpaceRequest[Namespace,Var,Tag,Value]
case class DFetchResponse[Namespace,Var,Tag,Value](
  path : CnxnCtxtLabel[Namespace,Var,Tag],
  value : Value
) extends DistributedTermSpaceResponse[Namespace,Var,Tag,Value]
case class DPutRequest[Namespace,Var,Tag,Value](
  path : CnxnCtxtLabel[Namespace,Var,Tag],
  value : Value
) extends DistributedTermSpaceRequest[Namespace,Var,Tag,Value]
case class DPutResponse[Namespace,Var,Tag,Value](
  path : CnxnCtxtLabel[Namespace,Var,Tag]
) extends DistributedTermSpaceResponse[Namespace,Var,Tag,Value]


trait DTSMSH[Namespace,Var,Tag,Value] {
  trait DistributedTermSpaceMsg[Namespace,Var,Tag,Value]

  trait DistributedTermSpaceRequest[Namespace,Var,Tag,Value]
     extends DistributedTermSpaceMsg[Namespace,Var,Tag,Value]

  case class DGetRequest[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag]
  ) extends DistributedTermSpaceRequest[Namespace,Var,Tag,Value]  
  case class DFetchRequest[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag]
  ) extends DistributedTermSpaceRequest[Namespace,Var,Tag,Value]
  case class DPutRequest[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) extends DistributedTermSpaceRequest[Namespace,Var,Tag,Value]

  trait DistributedTermSpaceResponse[Namespace,Var,Tag,Value]
     extends DistributedTermSpaceMsg[Namespace,Var,Tag,Value]

  case class DGetResponse[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) extends DistributedTermSpaceResponse[Namespace,Var,Tag,Value]
  case class DFetchResponse[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) extends DistributedTermSpaceResponse[Namespace,Var,Tag,Value]
  
  case class DPutResponse[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag]
  ) extends DistributedTermSpaceResponse[Namespace,Var,Tag,Value]

  type DTSMsg[Namespace,Var,Tag,Value] =
    DistributedTermSpaceMsg[Namespace,Var,Tag,Value]

  type DReq = DistributedTermSpaceRequest[Namespace,Var,Tag,Value]
  type DRsp = DistributedTermSpaceResponse[Namespace,Var,Tag,Value]
  type JTSReq = JustifiedRequest[DReq,DRsp]
  type JTSRsp = JustifiedResponse[DReq,DRsp]
  type JTSReqOrRsp = Either[JTSReq,JTSRsp]
  
  def protoDreq : DReq 
  val theProtoDreq : DReq = protoDreq
  def protoDrsp : DRsp 
  val theProtoDrsp : DRsp = protoDrsp
  def protoJtsreq : JTSReq
  val theProtoJtsreq : JTSReq = protoJtsreq
  def protoJtsrsp : JTSRsp
  val theProtoJtsrsp : JTSRsp = protoJtsrsp
  def protoJtsreqorrsp : JTSReqOrRsp
  val theProtoJtsreqorrsp : JTSReqOrRsp = protoJtsreqorrsp
}

trait DTSMsgScope[Namespace,Var,Tag,Value] {
  type MsgTypes <: DTSMSH[Namespace,Var,Tag,Value]
  def protoMsgs : MsgTypes
  val Msgs : MsgTypes = protoMsgs
}
