package com.biosimilarity.lift.lib.amqpJSONAPI2;
import com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Message.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Message,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LblReqHeader.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LblReqHeader,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LblRspHeader.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LblRspHeader,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LblReqBody.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LblReqBody,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LblRspBody.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LblRspBody,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.ReqHeader.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.ReqHeader,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.RspHeader.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.RspHeader,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRequest.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRequest,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBResponse.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBResponse,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AskReqPacket.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AskReqPacket,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AskRspPacket.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AskRspPacket,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.TellReqPacket.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.TellReqPacket,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.TellRspPacket.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.TellRspPacket,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.ReqJust.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.ReqJust,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.RspJust.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.RspJust,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AskReq.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AskReq,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.TellReq.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.TellReq,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AskRsp.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AskRsp,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.TellRsp.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.TellRsp,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Status.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Status,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Pattern.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Pattern,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Blob.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Blob,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Substitution.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Substitution,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.SubstPair.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.SubstPair,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryTerm.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryTerm,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryElem.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryElem,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryValue.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryValue,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryArray.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryArray,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryGrndLit.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryGrndLit,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryBool.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryBool,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryNum.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryNum,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URI.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URI,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIPath.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIPath,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URILocation.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URILocation,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRsrcLocation.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRsrcLocation,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRelativePath.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRelativePath,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRoot.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRoot,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.NetLocation.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.NetLocation,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIScheme.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIScheme,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIPathElement.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIPathElement,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.DNSElement.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.DNSElement,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Port.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Port,A>,
  com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.UUID.Visitor<com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.UUID,A>
{
/* Message */
    public Message visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustReqHB p, A arg)
    {
      LblReqHeader lblreqheader_ = p.lblreqheader_.accept(this, arg);
      LblReqBody lblreqbody_ = p.lblreqbody_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustReqHB(lblreqheader_, lblreqbody_);
    }
    public Message visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustReqBH p, A arg)
    {
      LblReqBody lblreqbody_ = p.lblreqbody_.accept(this, arg);
      LblReqHeader lblreqheader_ = p.lblreqheader_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustReqBH(lblreqbody_, lblreqheader_);
    }
    public Message visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustRspHB p, A arg)
    {
      LblRspHeader lblrspheader_ = p.lblrspheader_.accept(this, arg);
      LblRspBody lblrspbody_ = p.lblrspbody_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustRspHB(lblrspheader_, lblrspbody_);
    }
    public Message visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustRspBH p, A arg)
    {
      LblRspBody lblrspbody_ = p.lblrspbody_.accept(this, arg);
      LblRspHeader lblrspheader_ = p.lblrspheader_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustRspBH(lblrspbody_, lblrspheader_);
    }

/* LblReqHeader */
    public LblReqHeader visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblReqHdr p, A arg)
    {
      ReqHeader reqheader_ = p.reqheader_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblReqHdr(reqheader_);
    }

/* LblRspHeader */
    public LblRspHeader visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblRspHdr p, A arg)
    {
      RspHeader rspheader_ = p.rspheader_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblRspHdr(rspheader_);
    }

/* LblReqBody */
    public LblReqBody visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblReqBody p, A arg)
    {
      KVDBRequest kvdbrequest_ = p.kvdbrequest_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblReqBody(kvdbrequest_);
    }

/* LblRspBody */
    public LblRspBody visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblRspBody p, A arg)
    {
      KVDBResponse kvdbresponse_ = p.kvdbresponse_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblRspBody(kvdbresponse_);
    }

/* ReqHeader */
    public ReqHeader visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqHdr p, A arg)
    {
      URI uri_1 = p.uri_1.accept(this, arg);
      URI uri_2 = p.uri_2.accept(this, arg);
      UUID uuid_1 = p.uuid_1.accept(this, arg);
      UUID uuid_2 = p.uuid_2.accept(this, arg);
      ReqJust reqjust_ = p.reqjust_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqHdr(uri_1, uri_2, uuid_1, uuid_2, reqjust_);
    }
    public ReqHeader visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqNoHdr p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqNoHdr();
    }
    public ReqHeader visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqURIHdr p, A arg)
    {
      URI uri_ = p.uri_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqURIHdr(uri_);
    }
    public ReqHeader visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqUUIDHdr p, A arg)
    {
      UUID uuid_ = p.uuid_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqUUIDHdr(uuid_);
    }

/* RspHeader */
    public RspHeader visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspHdr p, A arg)
    {
      URI uri_1 = p.uri_1.accept(this, arg);
      URI uri_2 = p.uri_2.accept(this, arg);
      UUID uuid_1 = p.uuid_1.accept(this, arg);
      UUID uuid_2 = p.uuid_2.accept(this, arg);
      RspJust rspjust_ = p.rspjust_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspHdr(uri_1, uri_2, uuid_1, uuid_2, rspjust_);
    }
    public RspHeader visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspNoHdr p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspNoHdr();
    }
    public RspHeader visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspURIHdr p, A arg)
    {
      URI uri_ = p.uri_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspURIHdr(uri_);
    }
    public RspHeader visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspUUIDHdr p, A arg)
    {
      UUID uuid_ = p.uuid_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspUUIDHdr(uuid_);
    }

/* KVDBRequest */
    public KVDBRequest visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskReq p, A arg)
    {
      AskReq askreq_ = p.askreq_.accept(this, arg);
      AskReqPacket askreqpacket_ = p.askreqpacket_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskReq(askreq_, askreqpacket_);
    }
    public KVDBRequest visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellReq p, A arg)
    {
      TellReq tellreq_ = p.tellreq_.accept(this, arg);
      TellReqPacket tellreqpacket_ = p.tellreqpacket_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellReq(tellreq_, tellreqpacket_);
    }
    public KVDBRequest visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNoReq p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNoReq();
    }

/* KVDBResponse */
    public KVDBResponse visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskRsp p, A arg)
    {
      AskRsp askrsp_ = p.askrsp_.accept(this, arg);
      AskRspPacket askrsppacket_ = p.askrsppacket_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskRsp(askrsp_, askrsppacket_);
    }
    public KVDBResponse visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellRsp p, A arg)
    {
      TellRsp tellrsp_ = p.tellrsp_.accept(this, arg);
      TellRspPacket tellrsppacket_ = p.tellrsppacket_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellRsp(tellrsp_, tellrsppacket_);
    }
    public KVDBResponse visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNoRsp p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNoRsp();
    }

/* AskReqPacket */
    public AskReqPacket visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskReqData p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskReqData(pattern_);
    }

/* AskRspPacket */
    public AskRspPacket visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskRspData p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      Substitution substitution_ = p.substitution_.accept(this, arg);
      Blob blob_ = p.blob_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskRspData(pattern_, substitution_, blob_);
    }

/* TellReqPacket */
    public TellReqPacket visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellReqData p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      Blob blob_ = p.blob_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellReqData(pattern_, blob_);
    }

/* TellRspPacket */
    public TellRspPacket visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellRspData p, A arg)
    {
      Status status_ = p.status_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellRspData(status_);
    }

/* ReqJust */
    public ReqJust visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqJustNone p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqJustNone();
    }
    public ReqJust visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqJustSome p, A arg)
    {
      UUID uuid_ = p.uuid_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqJustSome(uuid_);
    }

/* RspJust */
    public RspJust visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspJustNone p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspJustNone();
    }
    public RspJust visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspJustSome p, A arg)
    {
      UUID uuid_ = p.uuid_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspJustSome(uuid_);
    }

/* AskReq */
    public AskReq visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBGetReq p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBGetReq();
    }
    public AskReq visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBFetchReq p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBFetchReq();
    }
    public AskReq visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubscribeReq p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubscribeReq();
    }

/* TellReq */
    public TellReq visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPutReq p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPutReq();
    }
    public TellReq visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStoreReq p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStoreReq();
    }
    public TellReq visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPublishReq p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPublishReq();
    }

/* AskRsp */
    public AskRsp visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBGetRsp p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBGetRsp();
    }
    public AskRsp visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBFetchRsp p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBFetchRsp();
    }
    public AskRsp visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubscribeRsp p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubscribeRsp();
    }

/* TellRsp */
    public TellRsp visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPutRsp p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPutRsp();
    }
    public TellRsp visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStoreRsp p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStoreRsp();
    }
    public TellRsp visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPublishRsp p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPublishRsp();
    }

/* Status */
    public Status visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusOk p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusOk();
    }
    public Status visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusNotOk p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusNotOk();
    }
    public Status visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusCode p, A arg)
    {
      Integer integer_ = p.integer_;

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusCode(integer_);
    }
    public Status visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusStr p, A arg)
    {
      String string_ = p.string_;

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusStr(string_);
    }

/* Pattern */
    public Pattern visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QPointed p, A arg)
    {
      QryTerm qryterm_ = p.qryterm_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QPointed(qryterm_);
    }

/* Blob */
    public Blob visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QBlob p, A arg)
    {
      String string_ = p.string_;

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QBlob(string_);
    }

/* Substitution */
    public Substitution visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubst p, A arg)
    {
      ListSubstPair listsubstpair_ = new ListSubstPair();
      for (SubstPair x : p.listsubstpair_) {
        listsubstpair_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubst(listsubstpair_);
    }

/* SubstPair */
    public SubstPair visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubstPair p, A arg)
    {
      String varuident_ = p.varuident_;
      QryTerm qryterm_ = p.qryterm_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubstPair(varuident_, qryterm_);
    }

/* QryTerm */
    public QryTerm visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QTerm p, A arg)
    {
      String string_ = p.string_;
      QryArray qryarray_ = p.qryarray_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QTerm(string_, qryarray_);
    }

/* QryElem */
    public QryElem visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QVar p, A arg)
    {
      String varuident_ = p.varuident_;

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QVar(varuident_);
    }
    public QryElem visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QVal p, A arg)
    {
      QryValue qryvalue_ = p.qryvalue_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QVal(qryvalue_);
    }

/* QryValue */
    public QryValue visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QAtomic p, A arg)
    {
      QryGrndLit qrygrndlit_ = p.qrygrndlit_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QAtomic(qrygrndlit_);
    }
    public QryValue visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QColl p, A arg)
    {
      QryArray qryarray_ = p.qryarray_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QColl(qryarray_);
    }
    public QryValue visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QComp p, A arg)
    {
      QryTerm qryterm_ = p.qryterm_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QComp(qryterm_);
    }

/* QryArray */
    public QryArray visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QArray p, A arg)
    {
      ListQryElem listqryelem_ = new ListQryElem();
      for (QryElem x : p.listqryelem_) {
        listqryelem_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QArray(listqryelem_);
    }

/* QryGrndLit */
    public QryGrndLit visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QStr p, A arg)
    {
      String string_ = p.string_;

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QStr(string_);
    }
    public QryGrndLit visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QNum p, A arg)
    {
      QryNum qrynum_ = p.qrynum_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QNum(qrynum_);
    }
    public QryGrndLit visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QBool p, A arg)
    {
      QryBool qrybool_ = p.qrybool_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QBool(qrybool_);
    }
    public QryGrndLit visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QNul p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QNul();
    }

/* QryBool */
    public QryBool visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QTru p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QTru();
    }
    public QryBool visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QFal p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QFal();
    }

/* QryNum */
    public QryNum visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QInt p, A arg)
    {
      Integer integer_ = p.integer_;

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QInt(integer_);
    }
    public QryNum visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QDbl p, A arg)
    {
      Double double_ = p.double_;

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QDbl(double_);
    }

/* URI */
    public URI visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.BasicURI p, A arg)
    {
      URIScheme urischeme_ = p.urischeme_.accept(this, arg);
      URIPath uripath_ = p.uripath_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.BasicURI(urischeme_, uripath_);
    }
    public URI visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.NullURI p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.NullURI();
    }

/* URIPath */
    public URIPath visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LocatedtedPath p, A arg)
    {
      URILocation urilocation_ = p.urilocation_.accept(this, arg);
      URIRelativePath urirelativepath_ = p.urirelativepath_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LocatedtedPath(urilocation_, urirelativepath_);
    }

/* URILocation */
    public URILocation visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URINetLocation p, A arg)
    {
      URIRoot uriroot_ = p.uriroot_.accept(this, arg);
      URIRsrcLocation urirsrclocation_ = p.urirsrclocation_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URINetLocation(uriroot_, urirsrclocation_);
    }

/* URIRsrcLocation */
    public URIRsrcLocation visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRsrcPortLoc p, A arg)
    {
      NetLocation netlocation_ = p.netlocation_.accept(this, arg);
      Port port_ = p.port_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRsrcPortLoc(netlocation_, port_);
    }
    public URIRsrcLocation visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRsrcLoc p, A arg)
    {
      NetLocation netlocation_ = p.netlocation_.accept(this, arg);

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRsrcLoc(netlocation_);
    }

/* URIRelativePath */
    public URIRelativePath visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.SlashPath p, A arg)
    {
      URIRoot uriroot_ = p.uriroot_.accept(this, arg);
      ListURIPathElement listuripathelement_ = new ListURIPathElement();
      for (URIPathElement x : p.listuripathelement_) {
        listuripathelement_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.SlashPath(uriroot_, listuripathelement_);
    }

/* URIRoot */
    public URIRoot visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIOrigin p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIOrigin();
    }

/* NetLocation */
    public NetLocation visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.DNSAddr p, A arg)
    {
      ListDNSElement listdnselement_ = new ListDNSElement();
      for (DNSElement x : p.listdnselement_) {
        listdnselement_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.DNSAddr(listdnselement_);
    }

/* URIScheme */
    public URIScheme visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomScheme p, A arg)
    {
      String lident_ = p.lident_;

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomScheme(lident_);
    }

/* URIPathElement */
    public URIPathElement visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomPathElement p, A arg)
    {
      String lident_ = p.lident_;

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomPathElement(lident_);
    }

/* DNSElement */
    public DNSElement visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomDNSElement p, A arg)
    {
      String lident_ = p.lident_;

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomDNSElement(lident_);
    }

/* Port */
    public Port visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomPort p, A arg)
    {
      Integer integer_ = p.integer_;

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomPort(integer_);
    }

/* UUID */
    public UUID visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLUUID p, A arg)
    {
      String lident_ = p.lident_;

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLUUID(lident_);
    }
    public UUID visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBUUUID p, A arg)
    {
      String uident_ = p.uident_;

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBUUUID(uident_);
    }
    public UUID visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNullUUID p, A arg)
    {

      return new com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNullUUID();
    }

}