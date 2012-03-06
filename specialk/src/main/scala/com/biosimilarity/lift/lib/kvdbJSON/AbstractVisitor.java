package com.biosimilarity.lift.lib.kvdbJSON;
import com.biosimilarity.lift.lib.kvdbJSON.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* Message */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqHB p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqBH p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspHB p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspBH p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Message p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* LblReqHeader */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqHdr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblReqHeader p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* LblRspHeader */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspHdr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblRspHeader p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* LblReqBody */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqBody p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblReqBody p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* LblRspBody */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspBody p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblRspBody p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ReqHeader */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqHdr p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqNoHdr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ReqHeader p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* RspHeader */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspHdr p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspNoHdr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.RspHeader p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* KVDBRequest */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReq p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReq p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoReq p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRequest p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* KVDBResponse */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRsp p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRsp p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoRsp p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBResponse p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* AskReqPacket */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReqData p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskReqPacket p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* AskRspPacket */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRspData p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskRspPacket p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* TellReqPacket */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReqData p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellReqPacket p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* TellRspPacket */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRspData p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellRspPacket p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ReqJust */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustNone p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustSome p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ReqJust p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* RspJust */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustNone p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustSome p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.RspJust p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* AskReq */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetReq p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchReq p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeReq p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskReq p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* TellReq */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutReq p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreReq p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishReq p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellReq p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* AskRsp */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetRsp p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchRsp p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeRsp p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskRsp p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* TellRsp */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutRsp p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreRsp p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishRsp p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellRsp p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Status */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusOk p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusNotOk p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusCode p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusStr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Status p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Pattern */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QPointed p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Pattern p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Blob */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBlob p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Blob p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Substitution */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubst p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Substitution p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* SubstPair */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubstPair p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.SubstPair p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QryTerm */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTerm p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryTerm p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QryElem */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVar p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVal p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryElem p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QryValue */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QAtomic p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QColl p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QComp p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryValue p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QryArray */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QArray p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryArray p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QryGrndLit */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QStr p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNum p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBool p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNul p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryGrndLit p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QryBool */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTru p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QFal p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryBool p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* QryNum */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QInt p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QDbl p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryNum p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URI */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TokenURI p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.BasicURI p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.NullURI p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URI p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URIPath */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LocatedtedPath p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.RelativePath p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIPath p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URILocation */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URINetLocation p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URILocation p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URIRsrcLocation */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcPortLoc p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLoc p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLocation p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URIRelativePath */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.SlashPath p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRelativePath p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URIRoot */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIOrigin p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRoot p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* NetLocation */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSAddr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.NetLocation p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URIScheme */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomScheme p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIScheme p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* URIPathElement */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPathElement p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIPathElement p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* DNSElement */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomDNSElement p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSElement p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Port */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPort p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Port p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* UUID */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLUUID p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPrimUUID p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNullUUID p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.kvdbJSON.Absyn.UUID p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
