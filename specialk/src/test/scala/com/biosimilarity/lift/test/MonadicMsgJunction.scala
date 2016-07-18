package com.biosimilarity.lift.test

import java.net.URI
import java.util.UUID

import com.biosimilarity.lift.lib.UUIDOps
import com.biosimilarity.lift.model.msg.{JustifiedRequest, JustifiedResponse}
import com.biosimilarity.lift.model.store.{CnxnCtxtLeaf, DTSMSH, MonadicDTSMsgScope}
import com.biosimilarity.lift.lib.moniker.identityConversions._

/**
  * lifted from MonadicJunction.scala
  */
object MonadicMsgJunction extends MonadicDTSMsgScope[String, String, String, String] with UUIDOps {

  type MsgTypes = DTSMSH[String, String, String, String]

  val aLabel = new CnxnCtxtLeaf[String, String, String](Left("a"))
  val bLabel = new CnxnCtxtLeaf[String, String, String](Left("b"))

  val protoDreqUUID: UUID = getUUID()
  val protoDrspUUID: UUID = getUUID()

  object MonadicDMsgs extends MsgTypes {

    override def protoDreq: DReq = MDGetRequest(aLabel)
    override def protoDrsp: DRsp = MDGetResponse(aLabel, aLabel.toString)

    val reqUri: URI = new URI("agent", protoDreqUUID.toString, "/invitation", "")
    val rspUri: URI = new URI("agent", protoDrspUUID.toString, "/invitation", "")

    override def protoJtsreq: JTSReq =
      JustifiedRequest(protoDreqUUID, reqUri, reqUri, getUUID(), protoDreq, None)

    override def protoJtsrsp: JTSRsp =
      JustifiedResponse(protoDreqUUID, rspUri, rspUri, getUUID(), protoDrsp, None)

    override def protoJtsreqorrsp: JTSReqOrRsp =
      Left(protoJtsreq)
  }

  override def protoMsgs: MsgTypes = MonadicDMsgs
}
