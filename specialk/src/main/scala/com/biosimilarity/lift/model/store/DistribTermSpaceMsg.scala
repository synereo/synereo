// -*- mode: Scala;-*- 
// Filename:    DistribTermSpaceMsg.scala 
// Authors:     lgm                                                    
// Creation:    Fri Jan 21 20:32:18 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._

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
  trait MDistributedTermSpaceMsg[Namespace,Var,Tag,Value]

  trait MDistributedTermSpaceRequest[Namespace,Var,Tag,Value]
     extends MDistributedTermSpaceMsg[Namespace,Var,Tag,Value]

  case class MDGetRequest[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag]
  ) extends MDistributedTermSpaceRequest[Namespace,Var,Tag,Value]  
  case class MDFetchRequest[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag]
  ) extends MDistributedTermSpaceRequest[Namespace,Var,Tag,Value]
  case class MDSubscribeRequest[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag]
  ) extends MDistributedTermSpaceRequest[Namespace,Var,Tag,Value]
  case class MDPutRequest[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) extends MDistributedTermSpaceRequest[Namespace,Var,Tag,Value]
  case class MDPublishRequest[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) extends MDistributedTermSpaceRequest[Namespace,Var,Tag,Value]

  trait MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
     extends MDistributedTermSpaceMsg[Namespace,Var,Tag,Value]

  case class MDGetResponse[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
  case class MDFetchResponse[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
  case class MDSubscribeResponse[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag],
    value : Value
  ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
  
  case class MDPutResponse[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag]
  ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
  case class MDPublishResponse[Namespace,Var,Tag,Value](
    path : CnxnCtxtLabel[Namespace,Var,Tag]
  ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]

  type DTSMsg[Namespace,Var,Tag,Value] =
    MDistributedTermSpaceMsg[Namespace,Var,Tag,Value]

  type DReq = MDistributedTermSpaceRequest[Namespace,Var,Tag,Value]
  type DRsp = MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
  type JTSReq = JustifiedRequest[DReq,DRsp]
  type JTSRsp = JustifiedResponse[DReq,DRsp]
  type JTSReqOrRsp = Either[JTSReq,JTSRsp]
  
  def protoDreq : DReq 
  lazy val theProtoDreq : DReq = protoDreq
  def protoDrsp : DRsp 
  lazy val theProtoDrsp : DRsp = protoDrsp
  def protoJtsreq : JTSReq
  lazy val theProtoJtsreq : JTSReq = protoJtsreq
  def protoJtsrsp : JTSRsp
  lazy val theProtoJtsrsp : JTSRsp = protoJtsrsp
  def protoJtsreqorrsp : JTSReqOrRsp
  lazy val theProtoJtsreqorrsp : JTSReqOrRsp = protoJtsreqorrsp
}

trait DTSMsgScope[Namespace,Var,Tag,Value] {
  type MsgTypes <: DTSMSH[Namespace,Var,Tag,Value]
  def protoMsgs : MsgTypes
  val Msgs : MsgTypes = protoMsgs
}

package usage { 
  object MonadicStringDMsgs
       extends DTSMSH[String,String,String,String]
       with UUIDOps
  {
    import identityConversions._
    
    val aLabelUUID = getUUID()
    val bLabelUUID = getUUID()
    
    val aLabel =
      new CnxnCtxtLeaf[String,String,String]( Left( aLabelUUID.toString ) )
    val bLabel =
      new CnxnCtxtLeaf[String,String,String]( Left( bLabelUUID.toString ) )
    
    val protoDreqUUID = getUUID()
    val protoDrspUUID = getUUID()    
    
    override def protoDreq : DReq = MDGetRequest( aLabel )
    override def protoDrsp : DRsp = MDGetResponse( aLabel, aLabel.toString )
    override def protoJtsreq : JTSReq =
      JustifiedRequest(
	protoDreqUUID,
	new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	getUUID(),
	protoDreq,
	None
      )
    override def protoJtsrsp : JTSRsp = 
      JustifiedResponse(
	protoDreqUUID,
	new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
	new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
	getUUID(),
	protoDrsp,
	None
      )
    override def protoJtsreqorrsp : JTSReqOrRsp =
      Left( protoJtsreq )
  }
}
