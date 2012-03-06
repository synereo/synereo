package com.biosimilarity.lift.lib.kvdbJSON;
import com.biosimilarity.lift.lib.kvdbJSON.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.Message.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.Message,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblReqHeader.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblReqHeader,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblRspHeader.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblRspHeader,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblReqBody.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblReqBody,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblRspBody.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblRspBody,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.ReqHeader.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.ReqHeader,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.RspHeader.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.RspHeader,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRequest.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRequest,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBResponse.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBResponse,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskReqPacket.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskReqPacket,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskRspPacket.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskRspPacket,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellReqPacket.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellReqPacket,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellRspPacket.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellRspPacket,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.ReqJust.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.ReqJust,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.RspJust.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.RspJust,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskReq.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskReq,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellReq.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellReq,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskRsp.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskRsp,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellRsp.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellRsp,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.Status.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.Status,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.Pattern.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.Pattern,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.Blob.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.Blob,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.Substitution.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.Substitution,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.SubstPair.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.SubstPair,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryTerm.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryTerm,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryElem.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryElem,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryValue.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryValue,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryArray.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryArray,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryGrndLit.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryGrndLit,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryBool.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryBool,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryNum.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryNum,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.URI.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.URI,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIPath.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIPath,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.URILocation.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.URILocation,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLocation.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLocation,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRelativePath.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRelativePath,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRoot.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRoot,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.NetLocation.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.NetLocation,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIScheme.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIScheme,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIPathElement.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIPathElement,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSElement.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSElement,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.Port.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.Port,A>,
  com.biosimilarity.lift.lib.kvdbJSON.Absyn.UUID.Visitor<com.biosimilarity.lift.lib.kvdbJSON.Absyn.UUID,A>
{
/* Message */
    public Message visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqHB p, A arg)
    {
      LblReqHeader lblreqheader_ = p.lblreqheader_.accept(this, arg);
      LblReqBody lblreqbody_ = p.lblreqbody_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqHB(lblreqheader_, lblreqbody_);
    }
    public Message visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqBH p, A arg)
    {
      LblReqBody lblreqbody_ = p.lblreqbody_.accept(this, arg);
      LblReqHeader lblreqheader_ = p.lblreqheader_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqBH(lblreqbody_, lblreqheader_);
    }
    public Message visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspHB p, A arg)
    {
      LblRspHeader lblrspheader_ = p.lblrspheader_.accept(this, arg);
      LblRspBody lblrspbody_ = p.lblrspbody_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspHB(lblrspheader_, lblrspbody_);
    }
    public Message visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspBH p, A arg)
    {
      LblRspBody lblrspbody_ = p.lblrspbody_.accept(this, arg);
      LblRspHeader lblrspheader_ = p.lblrspheader_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspBH(lblrspbody_, lblrspheader_);
    }

/* LblReqHeader */
    public LblReqHeader visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqHdr p, A arg)
    {
      ReqHeader reqheader_ = p.reqheader_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqHdr(reqheader_);
    }

/* LblRspHeader */
    public LblRspHeader visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspHdr p, A arg)
    {
      RspHeader rspheader_ = p.rspheader_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspHdr(rspheader_);
    }

/* LblReqBody */
    public LblReqBody visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqBody p, A arg)
    {
      KVDBRequest kvdbrequest_ = p.kvdbrequest_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqBody(kvdbrequest_);
    }

/* LblRspBody */
    public LblRspBody visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspBody p, A arg)
    {
      KVDBResponse kvdbresponse_ = p.kvdbresponse_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspBody(kvdbresponse_);
    }

/* ReqHeader */
    public ReqHeader visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqHdr p, A arg)
    {
      URI uri_1 = p.uri_1.accept(this, arg);
      URI uri_2 = p.uri_2.accept(this, arg);
      UUID uuid_1 = p.uuid_1.accept(this, arg);
      UUID uuid_2 = p.uuid_2.accept(this, arg);
      ReqJust reqjust_ = p.reqjust_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqHdr(uri_1, uri_2, uuid_1, uuid_2, reqjust_);
    }
    public ReqHeader visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqNoHdr p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqNoHdr();
    }

/* RspHeader */
    public RspHeader visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspHdr p, A arg)
    {
      URI uri_1 = p.uri_1.accept(this, arg);
      URI uri_2 = p.uri_2.accept(this, arg);
      UUID uuid_1 = p.uuid_1.accept(this, arg);
      UUID uuid_2 = p.uuid_2.accept(this, arg);
      RspJust rspjust_ = p.rspjust_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspHdr(uri_1, uri_2, uuid_1, uuid_2, rspjust_);
    }
    public RspHeader visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspNoHdr p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspNoHdr();
    }

/* KVDBRequest */
    public KVDBRequest visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReq p, A arg)
    {
      AskReq askreq_ = p.askreq_.accept(this, arg);
      AskReqPacket askreqpacket_ = p.askreqpacket_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReq(askreq_, askreqpacket_);
    }
    public KVDBRequest visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReq p, A arg)
    {
      TellReq tellreq_ = p.tellreq_.accept(this, arg);
      TellReqPacket tellreqpacket_ = p.tellreqpacket_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReq(tellreq_, tellreqpacket_);
    }
    public KVDBRequest visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoReq p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoReq();
    }

/* KVDBResponse */
    public KVDBResponse visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRsp p, A arg)
    {
      AskRsp askrsp_ = p.askrsp_.accept(this, arg);
      AskRspPacket askrsppacket_ = p.askrsppacket_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRsp(askrsp_, askrsppacket_);
    }
    public KVDBResponse visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRsp p, A arg)
    {
      TellRsp tellrsp_ = p.tellrsp_.accept(this, arg);
      TellRspPacket tellrsppacket_ = p.tellrsppacket_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRsp(tellrsp_, tellrsppacket_);
    }
    public KVDBResponse visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoRsp p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoRsp();
    }

/* AskReqPacket */
    public AskReqPacket visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReqData p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReqData(pattern_);
    }

/* AskRspPacket */
    public AskRspPacket visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRspData p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      Substitution substitution_ = p.substitution_.accept(this, arg);
      Blob blob_ = p.blob_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRspData(pattern_, substitution_, blob_);
    }

/* TellReqPacket */
    public TellReqPacket visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReqData p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      Blob blob_ = p.blob_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReqData(pattern_, blob_);
    }

/* TellRspPacket */
    public TellRspPacket visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRspData p, A arg)
    {
      Status status_ = p.status_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRspData(status_);
    }

/* ReqJust */
    public ReqJust visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustNone p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustNone();
    }
    public ReqJust visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustSome p, A arg)
    {
      UUID uuid_ = p.uuid_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustSome(uuid_);
    }

/* RspJust */
    public RspJust visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustNone p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustNone();
    }
    public RspJust visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustSome p, A arg)
    {
      UUID uuid_ = p.uuid_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustSome(uuid_);
    }

/* AskReq */
    public AskReq visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetReq p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetReq();
    }
    public AskReq visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchReq p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchReq();
    }
    public AskReq visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeReq p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeReq();
    }

/* TellReq */
    public TellReq visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutReq p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutReq();
    }
    public TellReq visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreReq p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreReq();
    }
    public TellReq visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishReq p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishReq();
    }

/* AskRsp */
    public AskRsp visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetRsp p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetRsp();
    }
    public AskRsp visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchRsp p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchRsp();
    }
    public AskRsp visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeRsp p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeRsp();
    }

/* TellRsp */
    public TellRsp visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutRsp p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutRsp();
    }
    public TellRsp visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreRsp p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreRsp();
    }
    public TellRsp visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishRsp p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishRsp();
    }

/* Status */
    public Status visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusOk p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusOk();
    }
    public Status visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusNotOk p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusNotOk();
    }
    public Status visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusCode p, A arg)
    {
      Integer integer_ = p.integer_;

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusCode(integer_);
    }
    public Status visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusStr p, A arg)
    {
      String string_ = p.string_;

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusStr(string_);
    }

/* Pattern */
    public Pattern visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QPointed p, A arg)
    {
      QryTerm qryterm_ = p.qryterm_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QPointed(qryterm_);
    }

/* Blob */
    public Blob visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBlob p, A arg)
    {
      String string_ = p.string_;

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBlob(string_);
    }

/* Substitution */
    public Substitution visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubst p, A arg)
    {
      ListSubstPair listsubstpair_ = new ListSubstPair();
      for (SubstPair x : p.listsubstpair_) {
        listsubstpair_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubst(listsubstpair_);
    }

/* SubstPair */
    public SubstPair visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubstPair p, A arg)
    {
      String varuident_ = p.varuident_;
      QryTerm qryterm_ = p.qryterm_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubstPair(varuident_, qryterm_);
    }

/* QryTerm */
    public QryTerm visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTerm p, A arg)
    {
      String string_ = p.string_;
      QryArray qryarray_ = p.qryarray_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTerm(string_, qryarray_);
    }

/* QryElem */
    public QryElem visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVar p, A arg)
    {
      String varuident_ = p.varuident_;

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVar(varuident_);
    }
    public QryElem visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVal p, A arg)
    {
      QryValue qryvalue_ = p.qryvalue_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVal(qryvalue_);
    }

/* QryValue */
    public QryValue visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QAtomic p, A arg)
    {
      QryGrndLit qrygrndlit_ = p.qrygrndlit_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QAtomic(qrygrndlit_);
    }
    public QryValue visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QColl p, A arg)
    {
      QryArray qryarray_ = p.qryarray_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QColl(qryarray_);
    }
    public QryValue visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QComp p, A arg)
    {
      QryTerm qryterm_ = p.qryterm_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QComp(qryterm_);
    }

/* QryArray */
    public QryArray visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QArray p, A arg)
    {
      ListQryElem listqryelem_ = new ListQryElem();
      for (QryElem x : p.listqryelem_) {
        listqryelem_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QArray(listqryelem_);
    }

/* QryGrndLit */
    public QryGrndLit visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QStr p, A arg)
    {
      String string_ = p.string_;

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QStr(string_);
    }
    public QryGrndLit visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNum p, A arg)
    {
      QryNum qrynum_ = p.qrynum_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNum(qrynum_);
    }
    public QryGrndLit visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBool p, A arg)
    {
      QryBool qrybool_ = p.qrybool_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBool(qrybool_);
    }
    public QryGrndLit visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNul p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNul();
    }

/* QryBool */
    public QryBool visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTru p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTru();
    }
    public QryBool visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QFal p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QFal();
    }

/* QryNum */
    public QryNum visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QInt p, A arg)
    {
      Integer integer_ = p.integer_;

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QInt(integer_);
    }
    public QryNum visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QDbl p, A arg)
    {
      Double double_ = p.double_;

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.QDbl(double_);
    }

/* URI */
    public URI visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TokenURI p, A arg)
    {
      String primuri_ = p.primuri_;

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.TokenURI(primuri_);
    }
    public URI visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.BasicURI p, A arg)
    {
      URIScheme urischeme_ = p.urischeme_.accept(this, arg);
      URIPath uripath_ = p.uripath_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.BasicURI(urischeme_, uripath_);
    }
    public URI visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.NullURI p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.NullURI();
    }

/* URIPath */
    public URIPath visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LocatedtedPath p, A arg)
    {
      URILocation urilocation_ = p.urilocation_.accept(this, arg);
      URIRelativePath urirelativepath_ = p.urirelativepath_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.LocatedtedPath(urilocation_, urirelativepath_);
    }
    public URIPath visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.RelativePath p, A arg)
    {
      URIRelativePath urirelativepath_ = p.urirelativepath_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.RelativePath(urirelativepath_);
    }

/* URILocation */
    public URILocation visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URINetLocation p, A arg)
    {
      URIRoot uriroot_ = p.uriroot_.accept(this, arg);
      URIRsrcLocation urirsrclocation_ = p.urirsrclocation_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.URINetLocation(uriroot_, urirsrclocation_);
    }

/* URIRsrcLocation */
    public URIRsrcLocation visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcPortLoc p, A arg)
    {
      NetLocation netlocation_ = p.netlocation_.accept(this, arg);
      Port port_ = p.port_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcPortLoc(netlocation_, port_);
    }
    public URIRsrcLocation visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLoc p, A arg)
    {
      NetLocation netlocation_ = p.netlocation_.accept(this, arg);

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLoc(netlocation_);
    }

/* URIRelativePath */
    public URIRelativePath visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.SlashPath p, A arg)
    {
      URIRoot uriroot_ = p.uriroot_.accept(this, arg);
      ListURIPathElement listuripathelement_ = new ListURIPathElement();
      for (URIPathElement x : p.listuripathelement_) {
        listuripathelement_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.SlashPath(uriroot_, listuripathelement_);
    }

/* URIRoot */
    public URIRoot visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIOrigin p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIOrigin();
    }

/* NetLocation */
    public NetLocation visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSAddr p, A arg)
    {
      ListDNSElement listdnselement_ = new ListDNSElement();
      for (DNSElement x : p.listdnselement_) {
        listdnselement_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSAddr(listdnselement_);
    }

/* URIScheme */
    public URIScheme visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomScheme p, A arg)
    {
      String lident_ = p.lident_;

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomScheme(lident_);
    }

/* URIPathElement */
    public URIPathElement visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPathElement p, A arg)
    {
      String lident_ = p.lident_;

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPathElement(lident_);
    }

/* DNSElement */
    public DNSElement visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomDNSElement p, A arg)
    {
      String lident_ = p.lident_;

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomDNSElement(lident_);
    }

/* Port */
    public Port visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPort p, A arg)
    {
      Integer integer_ = p.integer_;

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPort(integer_);
    }

/* UUID */
    public UUID visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLUUID p, A arg)
    {
      String lident_ = p.lident_;

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLUUID(lident_);
    }
    public UUID visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPrimUUID p, A arg)
    {
      String primuuid_ = p.primuuid_;

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPrimUUID(primuuid_);
    }
    public UUID visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNullUUID p, A arg)
    {

      return new com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNullUUID();
    }

}