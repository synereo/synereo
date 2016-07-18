package com.biosimilarity.lift.test

import java.net.URI
import java.util.UUID

import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker.identityConversions._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.model.store.{CnxnCtxtLeaf, DTSMSH}

/**
  * lifted from DistribTermSpaceMsg.scala
  */
object MonadicStringDMsgs extends DTSMSH[String, String, String, String] with UUIDOps with Serializable {

  type CCL = CnxnCtxtLeaf[String, String, String]

  val aLabelUUID: UUID = getUUID()
  val bLabelUUID: UUID = getUUID()

  val aLabel: CCL = new CnxnCtxtLeaf[String, String, String](Left(aLabelUUID.toString))
  val bLabel: CCL = new CnxnCtxtLeaf[String, String, String](Left(bLabelUUID.toString))

  val protoDreqUUID: UUID = getUUID()
  val protoDrspUUID: UUID = getUUID()

  val reqUri: URI = new URI("agent", protoDreqUUID.toString, "/invitation", "")
  val rspUri: URI = new URI("agent", protoDrspUUID.toString, "/invitation", "")

  override def protoDreq: DReq               = MDGetRequest(aLabel)
  override def protoDrsp: DRsp               = MDGetResponse(aLabel, aLabel.toString)
  override def protoJtsreq: JTSReq           = JustifiedRequest(protoDreqUUID, reqUri, reqUri, getUUID(), protoDreq, None)
  override def protoJtsrsp: JTSRsp           = JustifiedResponse(protoDrspUUID, rspUri, rspUri, getUUID(), protoDrsp, None)
  override def protoJtsreqorrsp: JTSReqOrRsp = Left(protoJtsreq)
}
