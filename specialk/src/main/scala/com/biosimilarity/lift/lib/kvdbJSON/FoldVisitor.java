package com.biosimilarity.lift.lib.kvdbJSON;

import com.biosimilarity.lift.lib.kvdbJSON.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* Message */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqHB p, A arg) {
      R r = leaf(arg);
      r = combine(p.lblreqheader_.accept(this, arg), r, arg);
      r = combine(p.lblreqbody_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqBH p, A arg) {
      R r = leaf(arg);
      r = combine(p.lblreqbody_.accept(this, arg), r, arg);
      r = combine(p.lblreqheader_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspHB p, A arg) {
      R r = leaf(arg);
      r = combine(p.lblrspheader_.accept(this, arg), r, arg);
      r = combine(p.lblrspbody_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspBH p, A arg) {
      R r = leaf(arg);
      r = combine(p.lblrspbody_.accept(this, arg), r, arg);
      r = combine(p.lblrspheader_.accept(this, arg), r, arg);
      return r;
    }

/* LblReqHeader */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqHdr p, A arg) {
      R r = leaf(arg);
      r = combine(p.reqheader_.accept(this, arg), r, arg);
      return r;
    }

/* LblRspHeader */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspHdr p, A arg) {
      R r = leaf(arg);
      r = combine(p.rspheader_.accept(this, arg), r, arg);
      return r;
    }

/* LblReqBody */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqBody p, A arg) {
      R r = leaf(arg);
      r = combine(p.kvdbrequest_.accept(this, arg), r, arg);
      return r;
    }

/* LblRspBody */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspBody p, A arg) {
      R r = leaf(arg);
      r = combine(p.kvdbresponse_.accept(this, arg), r, arg);
      return r;
    }

/* ReqHeader */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqHdr p, A arg) {
      R r = leaf(arg);
      r = combine(p.uri_1.accept(this, arg), r, arg);
      r = combine(p.uri_2.accept(this, arg), r, arg);
      r = combine(p.uuid_1.accept(this, arg), r, arg);
      r = combine(p.uuid_2.accept(this, arg), r, arg);
      r = combine(p.reqjust_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqNoHdr p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* RspHeader */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspHdr p, A arg) {
      R r = leaf(arg);
      r = combine(p.uri_1.accept(this, arg), r, arg);
      r = combine(p.uri_2.accept(this, arg), r, arg);
      r = combine(p.uuid_1.accept(this, arg), r, arg);
      r = combine(p.uuid_2.accept(this, arg), r, arg);
      r = combine(p.rspjust_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspNoHdr p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* KVDBRequest */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReq p, A arg) {
      R r = leaf(arg);
      r = combine(p.askreq_.accept(this, arg), r, arg);
      r = combine(p.askreqpacket_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReq p, A arg) {
      R r = leaf(arg);
      r = combine(p.tellreq_.accept(this, arg), r, arg);
      r = combine(p.tellreqpacket_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoReq p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* KVDBResponse */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRsp p, A arg) {
      R r = leaf(arg);
      r = combine(p.askrsp_.accept(this, arg), r, arg);
      r = combine(p.askrsppacket_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRsp p, A arg) {
      R r = leaf(arg);
      r = combine(p.tellrsp_.accept(this, arg), r, arg);
      r = combine(p.tellrsppacket_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoRsp p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* AskReqPacket */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReqData p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      return r;
    }

/* AskRspPacket */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRspData p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      r = combine(p.substitution_.accept(this, arg), r, arg);
      r = combine(p.blob_.accept(this, arg), r, arg);
      return r;
    }

/* TellReqPacket */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReqData p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      r = combine(p.blob_.accept(this, arg), r, arg);
      return r;
    }

/* TellRspPacket */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRspData p, A arg) {
      R r = leaf(arg);
      r = combine(p.status_.accept(this, arg), r, arg);
      return r;
    }

/* ReqJust */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustNone p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustSome p, A arg) {
      R r = leaf(arg);
      r = combine(p.uuid_.accept(this, arg), r, arg);
      return r;
    }

/* RspJust */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustNone p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustSome p, A arg) {
      R r = leaf(arg);
      r = combine(p.uuid_.accept(this, arg), r, arg);
      return r;
    }

/* AskReq */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetReq p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchReq p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeReq p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* TellReq */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutReq p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreReq p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishReq p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* AskRsp */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetRsp p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchRsp p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeRsp p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* TellRsp */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutRsp p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreRsp p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishRsp p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Status */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusOk p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusNotOk p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusCode p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusStr p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Pattern */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QPointed p, A arg) {
      R r = leaf(arg);
      r = combine(p.qryterm_.accept(this, arg), r, arg);
      return r;
    }

/* Blob */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBlob p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Substitution */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubst p, A arg) {
      R r = leaf(arg);
      for (SubstPair x : p.listsubstpair_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* SubstPair */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubstPair p, A arg) {
      R r = leaf(arg);
      r = combine(p.qryterm_.accept(this, arg), r, arg);
      return r;
    }

/* QryTerm */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTerm p, A arg) {
      R r = leaf(arg);
      r = combine(p.qryarray_.accept(this, arg), r, arg);
      return r;
    }

/* QryElem */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVal p, A arg) {
      R r = leaf(arg);
      r = combine(p.qryvalue_.accept(this, arg), r, arg);
      return r;
    }

/* QryValue */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QAtomic p, A arg) {
      R r = leaf(arg);
      r = combine(p.qrygrndlit_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QColl p, A arg) {
      R r = leaf(arg);
      r = combine(p.qryarray_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QComp p, A arg) {
      R r = leaf(arg);
      r = combine(p.qryterm_.accept(this, arg), r, arg);
      return r;
    }

/* QryArray */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QArray p, A arg) {
      R r = leaf(arg);
      for (QryElem x : p.listqryelem_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* QryGrndLit */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QStr p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNum p, A arg) {
      R r = leaf(arg);
      r = combine(p.qrynum_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBool p, A arg) {
      R r = leaf(arg);
      r = combine(p.qrybool_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNul p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* QryBool */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTru p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QFal p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* QryNum */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QInt p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QDbl p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* URI */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TokenURI p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.BasicURI p, A arg) {
      R r = leaf(arg);
      r = combine(p.urischeme_.accept(this, arg), r, arg);
      r = combine(p.uripath_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.NullURI p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* URIPath */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LocatedtedPath p, A arg) {
      R r = leaf(arg);
      r = combine(p.urilocation_.accept(this, arg), r, arg);
      r = combine(p.urirelativepath_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.RelativePath p, A arg) {
      R r = leaf(arg);
      r = combine(p.urirelativepath_.accept(this, arg), r, arg);
      return r;
    }

/* URILocation */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URINetLocation p, A arg) {
      R r = leaf(arg);
      r = combine(p.uriroot_.accept(this, arg), r, arg);
      r = combine(p.urirsrclocation_.accept(this, arg), r, arg);
      return r;
    }

/* URIRsrcLocation */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcPortLoc p, A arg) {
      R r = leaf(arg);
      r = combine(p.netlocation_.accept(this, arg), r, arg);
      r = combine(p.port_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLoc p, A arg) {
      R r = leaf(arg);
      r = combine(p.netlocation_.accept(this, arg), r, arg);
      return r;
    }

/* URIRelativePath */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.SlashPath p, A arg) {
      R r = leaf(arg);
      r = combine(p.uriroot_.accept(this, arg), r, arg);
      for (URIPathElement x : p.listuripathelement_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* URIRoot */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIOrigin p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* NetLocation */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSAddr p, A arg) {
      R r = leaf(arg);
      for (DNSElement x : p.listdnselement_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* URIScheme */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomScheme p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* URIPathElement */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPathElement p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* DNSElement */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomDNSElement p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Port */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPort p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* UUID */
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLUUID p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPrimUUID p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNullUUID p, A arg) {
      R r = leaf(arg);
      return r;
    }


}
