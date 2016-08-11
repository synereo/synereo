// -*- mode: Scala;-*- 
// Filename:    CnxnMsgs.scala 
// Authors:     lgm                                                    
// Creation:    Mon Apr  4 02:45:53 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.store

import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._

trait CnxnDTSMSH[Namespace,Var,Tag,Value]
extends DTSMsgScope[Namespace,Var,Tag,Value]
with AgentCnxnTypeScope {
  trait CnxnMDistributedTermSpaceMsg {
    def cnxn : acT.AgentCnxn
  }

  case class CnxnMDGetRequest[Namespace,Var,Tag,Value](
    override val cnxn : acT.AgentCnxn,
    path : CnxnCtxtLabel[Namespace,Var,Tag]
  ) extends Msgs.MDistributedTermSpaceRequest[Namespace,Var,Tag,Value]  
       with CnxnMDistributedTermSpaceMsg
  case class CnxnMDFetchRequest[Namespace,Var,Tag,Value](
    override val cnxn : acT.AgentCnxn,
    path : CnxnCtxtLabel[Namespace,Var,Tag]    
  ) extends Msgs.MDistributedTermSpaceRequest[Namespace,Var,Tag,Value]
       with CnxnMDistributedTermSpaceMsg
  case class CnxnMDSubscribeRequest[Namespace,Var,Tag,Value](
    override val cnxn : acT.AgentCnxn,
    path : CnxnCtxtLabel[Namespace,Var,Tag]    
  ) extends Msgs.MDistributedTermSpaceRequest[Namespace,Var,Tag,Value]
       with CnxnMDistributedTermSpaceMsg
  
  case class CnxnMDGetResponse[Namespace,Var,Tag,Value](
    override val cnxn : acT.AgentCnxn,
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) extends Msgs.MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
       with CnxnMDistributedTermSpaceMsg
  case class CnxnMDFetchResponse[Namespace,Var,Tag,Value](
    override val cnxn : acT.AgentCnxn,
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) extends Msgs.MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
       with CnxnMDistributedTermSpaceMsg
  case class CnxnMDSubscribeResponse[Namespace,Var,Tag,Value](
    override val cnxn : acT.AgentCnxn,
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) extends Msgs.MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
       with CnxnMDistributedTermSpaceMsg
  
  type CnxnDReq =
    Msgs.MDistributedTermSpaceRequest[Namespace,Var,Tag,Value]
       with CnxnMDistributedTermSpaceMsg
  type CnxnDRsp =
    Msgs.MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
       with CnxnMDistributedTermSpaceMsg
  
  type CnxnJTSReq = JustifiedRequest[CnxnDReq,CnxnDRsp]
  type CnxnJTSRsp = JustifiedResponse[CnxnDReq,CnxnDRsp]
  type CnxnJTSReqOrRsp = Either[CnxnJTSReq,CnxnJTSRsp]
  
  def protoCnxnDreq : CnxnDReq 
  lazy val theProtoCnxnDreq : CnxnDReq = protoCnxnDreq
  def protoCnxnDrsp : CnxnDRsp 
  lazy val theProtoCnxnDrsp : CnxnDRsp = protoCnxnDrsp
  def protoCnxnJtsreq : CnxnJTSReq
  lazy val theProtoCnxnJtsreq : CnxnJTSReq = protoCnxnJtsreq
  def protoCnxnJtsrsp : CnxnJTSRsp
  lazy val theProtoCnxnJtsrsp : CnxnJTSRsp = protoCnxnJtsrsp
  def protoCnxnJtsreqorrsp : CnxnJTSReqOrRsp
  lazy val theProtoCnxnJtsreqorrsp : CnxnJTSReqOrRsp = protoCnxnJtsreqorrsp
}

trait CnxnDTSMsgScope[Namespace,Var,Tag,Value] {
  type CnxnMsgTypes <: CnxnDTSMSH[Namespace,Var,Tag,Value]
  def protoCnxnMsgs : CnxnMsgTypes
  val CnxnMsgs : CnxnMsgTypes = protoCnxnMsgs
}
