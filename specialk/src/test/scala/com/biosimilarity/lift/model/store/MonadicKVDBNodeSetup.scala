package com.biosimilarity.lift.model.store

import java.net.URI

import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker.identityConversions._
import com.biosimilarity.lift.model.msg._

object MonadicKVDBNodeSetup extends MonadicKVDBNodeScope[String, String, String, String] with UUIDOps with Serializable {

  type MTTypes = MonadicTermTypes[String, String, String, String]

  object TheMTT extends MTTypes with Serializable

  override def protoTermTypes: MTTypes = TheMTT

  type DATypes = DistributedAskTypes

  object TheDAT extends DATypes with Serializable

  override def protoAskTypes: DATypes = TheDAT

  override type MsgTypes     = DTSMSHRsrc
  override type RsrcMsgTypes = DTSMSHRsrc

  val protoDreqUUID = getUUID()
  val protoDrspUUID = getUUID()

  lazy val aLabel = new CnxnCtxtLeaf[String, String, String](Left("a"))

  val reqUri: URI = new URI("agent", protoDreqUUID.toString, "/invitation", "")
  val rspUri: URI = new URI("agent", protoDrspUUID.toString, "/invitation", "")

  object MonadicDRsrcMsgs extends RsrcMsgTypes with Serializable {
    override def protoDreq: DReq               = MDGetRequest(aLabel)
    override def protoDrsp: DRsp               = MDGetResponse(aLabel, "")
    override def protoJtsreq: JTSReq           = JustifiedRequest(protoDreqUUID, reqUri, reqUri, getUUID(), protoDreq, None)
    override def protoJtsrsp: JTSRsp           = JustifiedResponse(protoDreqUUID, rspUri, rspUri, getUUID(), protoDrsp, None)
    override def protoJtsreqorrsp: JTSReqOrRsp = Left(protoJtsreq)
  }

  override def protoMsgs: MsgTypes         = MonadicDRsrcMsgs
  override def protoRsrcMsgs: RsrcMsgTypes = MonadicDRsrcMsgs

  def setup[ReqBody <: KVDBNodeRequest, RspBody <: KVDBNodeResponse](localHost: String,
                                                                     localPort: Int,
                                                                     remoteHost: String,
                                                                     remotePort: Int)(implicit returnTwist: Boolean = false)
    : Either[MonadicKVDBNode[ReqBody, RspBody], (MonadicKVDBNode[ReqBody, RspBody], MonadicKVDBNode[ReqBody, RspBody])] = {
    val (localExchange, remoteExchange) = if (localHost.equals(remoteHost) && (localPort == remotePort)) {
      ("/xmlDbUseCaseProtocolLocal", "/xmlDbUseCaseProtocolRemote")
    } else {
      ("/xmlDbUseCaseProtocol", "/xmlDbUseCaseProtocol")
    }
    if (returnTwist) {
      Right[MonadicKVDBNode[ReqBody, RspBody], (MonadicKVDBNode[ReqBody, RspBody], MonadicKVDBNode[ReqBody, RspBody])](
        (KVDBNodeFactory.ptToPt(new URI("agent", null, localHost, localPort, localExchange, null, null),
                                new URI("agent", null, remoteHost, remotePort, remoteExchange, null, null)),
         KVDBNodeFactory.ptToPt(new URI("agent", null, remoteHost, remotePort, remoteExchange, null, null),
                                new URI("agent", null, localHost, localPort, localExchange, null, null))))
    } else {
      Left[MonadicKVDBNode[ReqBody, RspBody], (MonadicKVDBNode[ReqBody, RspBody], MonadicKVDBNode[ReqBody, RspBody])](
        KVDBNodeFactory.ptToPt(new URI("agent", null, localHost, localPort, localExchange, null, null),
                               new URI("agent", null, remoteHost, remotePort, remoteExchange, null, null)))
    }
  }
}
