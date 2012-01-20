package com.biosimilarity.lift.lib.amqpJSONAPI2;
import com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* Message */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustReqHB p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustReqBH p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustRspHB p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustRspBH p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Message p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* LblReqHeader */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblReqHdr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LblReqHeader p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* LblRspHeader */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblRspHdr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LblRspHeader p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* LblReqBody */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblReqBody p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LblReqBody p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* LblRspBody */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblRspBody p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LblRspBody p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ReqHeader */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqHdr p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqNoHdr p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqURIHdr p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqUUIDHdr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.ReqHeader p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* RspHeader */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspHdr p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspNoHdr p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspURIHdr p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspUUIDHdr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.RspHeader p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* KVDBRequest */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskReq p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellReq p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNoReq p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRequest p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* KVDBResponse */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskRsp p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellRsp p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNoRsp p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBResponse p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* AskReqPacket */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskReqData p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AskReqPacket p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* AskRspPacket */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskRspData p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AskRspPacket p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* TellReqPacket */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellReqData p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.TellReqPacket p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* TellRspPacket */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellRspData p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.TellRspPacket p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ReqJust */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqJustNone p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqJustSome p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.ReqJust p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* RspJust */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspJustNone p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspJustSome p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.RspJust p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* AskReq */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBGetReq p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBFetchReq p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubscribeReq p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AskReq p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* TellReq */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPutReq p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStoreReq p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPublishReq p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.TellReq p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* AskRsp */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBGetRsp p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBFetchRsp p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubscribeRsp p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AskRsp p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* TellRsp */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPutRsp p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStoreRsp p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPublishRsp p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.TellRsp p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Status */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusOk p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusNotOk p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusCode p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusStr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Status p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Pattern */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QPointed p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Pattern p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Blob */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QBlob p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Blob p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Substitution */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubst p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Substitution p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* SubstPair */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubstPair p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.SubstPair p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QryTerm */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QTerm p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryTerm p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QryElem */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QVar p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QVal p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryElem p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QryValue */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QAtomic p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QColl p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QComp p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryValue p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QryArray */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QArray p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryArray p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QryGrndLit */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QStr p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QNum p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QBool p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QNul p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryGrndLit p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QryBool */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QTru p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QFal p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryBool p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QryNum */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QInt p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QDbl p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QryNum p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URI */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.BasicURI p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.NullURI p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URI p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URIPath */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LocatedtedPath p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIPath p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URILocation */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URINetLocation p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URILocation p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URIRsrcLocation */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRsrcPortLoc p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRsrcLoc p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRsrcLocation p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URIRelativePath */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.SlashPath p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRelativePath p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URIRoot */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIOrigin p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRoot p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* NetLocation */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.DNSAddr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.NetLocation p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URIScheme */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomScheme p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIScheme p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URIPathElement */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomPathElement p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIPathElement p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* DNSElement */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomDNSElement p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.DNSElement p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Port */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomPort p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.Port p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* UUID */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLUUID p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBUUUID p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNullUUID p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.UUID p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
