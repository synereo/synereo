package com.biosimilarity.lift.lib.amqpJSONAPI2;

import com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* Message */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustReqHB p, A arg) {
      R r = leaf(arg);
      r = combine(p.lblreqheader_.accept(this, arg), r, arg);
      r = combine(p.lblreqbody_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustReqBH p, A arg) {
      R r = leaf(arg);
      r = combine(p.lblreqbody_.accept(this, arg), r, arg);
      r = combine(p.lblreqheader_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustRspHB p, A arg) {
      R r = leaf(arg);
      r = combine(p.lblrspheader_.accept(this, arg), r, arg);
      r = combine(p.lblrspbody_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustRspBH p, A arg) {
      R r = leaf(arg);
      r = combine(p.lblrspbody_.accept(this, arg), r, arg);
      r = combine(p.lblrspheader_.accept(this, arg), r, arg);
      return r;
    }

/* LblReqHeader */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblReqHdr p, A arg) {
      R r = leaf(arg);
      r = combine(p.reqheader_.accept(this, arg), r, arg);
      return r;
    }

/* LblRspHeader */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblRspHdr p, A arg) {
      R r = leaf(arg);
      r = combine(p.rspheader_.accept(this, arg), r, arg);
      return r;
    }

/* LblReqBody */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblReqBody p, A arg) {
      R r = leaf(arg);
      r = combine(p.kvdbrequest_.accept(this, arg), r, arg);
      return r;
    }

/* LblRspBody */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblRspBody p, A arg) {
      R r = leaf(arg);
      r = combine(p.kvdbresponse_.accept(this, arg), r, arg);
      return r;
    }

/* ReqHeader */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqHdr p, A arg) {
      R r = leaf(arg);
      r = combine(p.uri_1.accept(this, arg), r, arg);
      r = combine(p.uri_2.accept(this, arg), r, arg);
      r = combine(p.uuid_1.accept(this, arg), r, arg);
      r = combine(p.uuid_2.accept(this, arg), r, arg);
      r = combine(p.reqjust_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqNoHdr p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqURIHdr p, A arg) {
      R r = leaf(arg);
      r = combine(p.uri_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqUUIDHdr p, A arg) {
      R r = leaf(arg);
      r = combine(p.uuid_.accept(this, arg), r, arg);
      return r;
    }

/* RspHeader */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspHdr p, A arg) {
      R r = leaf(arg);
      r = combine(p.uri_1.accept(this, arg), r, arg);
      r = combine(p.uri_2.accept(this, arg), r, arg);
      r = combine(p.uuid_1.accept(this, arg), r, arg);
      r = combine(p.uuid_2.accept(this, arg), r, arg);
      r = combine(p.rspjust_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspNoHdr p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspURIHdr p, A arg) {
      R r = leaf(arg);
      r = combine(p.uri_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspUUIDHdr p, A arg) {
      R r = leaf(arg);
      r = combine(p.uuid_.accept(this, arg), r, arg);
      return r;
    }

/* KVDBRequest */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskReq p, A arg) {
      R r = leaf(arg);
      r = combine(p.askreq_.accept(this, arg), r, arg);
      r = combine(p.askreqpacket_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellReq p, A arg) {
      R r = leaf(arg);
      r = combine(p.tellreq_.accept(this, arg), r, arg);
      r = combine(p.tellreqpacket_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNoReq p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* KVDBResponse */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskRsp p, A arg) {
      R r = leaf(arg);
      r = combine(p.askrsp_.accept(this, arg), r, arg);
      r = combine(p.askrsppacket_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellRsp p, A arg) {
      R r = leaf(arg);
      r = combine(p.tellrsp_.accept(this, arg), r, arg);
      r = combine(p.tellrsppacket_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNoRsp p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* AskReqPacket */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskReqData p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      return r;
    }

/* AskRspPacket */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskRspData p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      r = combine(p.substitution_.accept(this, arg), r, arg);
      r = combine(p.blob_.accept(this, arg), r, arg);
      return r;
    }

/* TellReqPacket */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellReqData p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      r = combine(p.blob_.accept(this, arg), r, arg);
      return r;
    }

/* TellRspPacket */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellRspData p, A arg) {
      R r = leaf(arg);
      r = combine(p.status_.accept(this, arg), r, arg);
      return r;
    }

/* ReqJust */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqJustNone p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqJustSome p, A arg) {
      R r = leaf(arg);
      r = combine(p.uuid_.accept(this, arg), r, arg);
      return r;
    }

/* RspJust */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspJustNone p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspJustSome p, A arg) {
      R r = leaf(arg);
      r = combine(p.uuid_.accept(this, arg), r, arg);
      return r;
    }

/* AskReq */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBGetReq p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBFetchReq p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubscribeReq p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* TellReq */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPutReq p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStoreReq p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPublishReq p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* AskRsp */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBGetRsp p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBFetchRsp p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubscribeRsp p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* TellRsp */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPutRsp p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStoreRsp p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPublishRsp p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Status */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusOk p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusNotOk p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusCode p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusStr p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Pattern */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QPointed p, A arg) {
      R r = leaf(arg);
      r = combine(p.qryterm_.accept(this, arg), r, arg);
      return r;
    }

/* Blob */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QBlob p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Substitution */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubst p, A arg) {
      R r = leaf(arg);
      for (SubstPair x : p.listsubstpair_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* SubstPair */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubstPair p, A arg) {
      R r = leaf(arg);
      r = combine(p.qryterm_.accept(this, arg), r, arg);
      return r;
    }

/* QryTerm */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QTerm p, A arg) {
      R r = leaf(arg);
      r = combine(p.qryarray_.accept(this, arg), r, arg);
      return r;
    }

/* QryElem */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QVar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QVal p, A arg) {
      R r = leaf(arg);
      r = combine(p.qryvalue_.accept(this, arg), r, arg);
      return r;
    }

/* QryValue */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QAtomic p, A arg) {
      R r = leaf(arg);
      r = combine(p.qrygrndlit_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QColl p, A arg) {
      R r = leaf(arg);
      r = combine(p.qryarray_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QComp p, A arg) {
      R r = leaf(arg);
      r = combine(p.qryterm_.accept(this, arg), r, arg);
      return r;
    }

/* QryArray */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QArray p, A arg) {
      R r = leaf(arg);
      for (QryElem x : p.listqryelem_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* QryGrndLit */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QStr p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QNum p, A arg) {
      R r = leaf(arg);
      r = combine(p.qrynum_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QBool p, A arg) {
      R r = leaf(arg);
      r = combine(p.qrybool_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QNul p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* QryBool */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QTru p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QFal p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* QryNum */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QInt p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QDbl p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* URI */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.BasicURI p, A arg) {
      R r = leaf(arg);
      r = combine(p.urischeme_.accept(this, arg), r, arg);
      r = combine(p.uripath_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.NullURI p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* URIPath */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LocatedtedPath p, A arg) {
      R r = leaf(arg);
      r = combine(p.urilocation_.accept(this, arg), r, arg);
      r = combine(p.urirelativepath_.accept(this, arg), r, arg);
      return r;
    }

/* URILocation */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URINetLocation p, A arg) {
      R r = leaf(arg);
      r = combine(p.uriroot_.accept(this, arg), r, arg);
      r = combine(p.urirsrclocation_.accept(this, arg), r, arg);
      return r;
    }

/* URIRsrcLocation */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRsrcPortLoc p, A arg) {
      R r = leaf(arg);
      r = combine(p.netlocation_.accept(this, arg), r, arg);
      r = combine(p.port_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRsrcLoc p, A arg) {
      R r = leaf(arg);
      r = combine(p.netlocation_.accept(this, arg), r, arg);
      return r;
    }

/* URIRelativePath */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.SlashPath p, A arg) {
      R r = leaf(arg);
      r = combine(p.uriroot_.accept(this, arg), r, arg);
      for (URIPathElement x : p.listuripathelement_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* URIRoot */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIOrigin p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* NetLocation */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.DNSAddr p, A arg) {
      R r = leaf(arg);
      for (DNSElement x : p.listdnselement_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* URIScheme */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomScheme p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* URIPathElement */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomPathElement p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* DNSElement */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomDNSElement p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Port */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomPort p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* UUID */
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLUUID p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBUUUID p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNullUUID p, A arg) {
      R r = leaf(arg);
      return r;
    }


}
