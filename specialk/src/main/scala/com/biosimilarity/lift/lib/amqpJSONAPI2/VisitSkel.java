package com.biosimilarity.lift.lib.amqpJSONAPI2;
import com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.*;
/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/
/* This implements the common visitor design pattern.
   Tests show it to be slightly less efficient than the
   instanceof method, but easier to use. 
   Replace the R and A parameters with the desired return
   and context types.*/

public class VisitSkel
{
  public class MessageVisitor<R,A> implements Message.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustReqHB p, A arg)
    {
      /* Code For KVDBJustReqHB Goes Here */

      p.lblreqheader_.accept(new LblReqHeaderVisitor<R,A>(), arg);
      p.lblreqbody_.accept(new LblReqBodyVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustReqBH p, A arg)
    {
      /* Code For KVDBJustReqBH Goes Here */

      p.lblreqbody_.accept(new LblReqBodyVisitor<R,A>(), arg);
      p.lblreqheader_.accept(new LblReqHeaderVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustRspHB p, A arg)
    {
      /* Code For KVDBJustRspHB Goes Here */

      p.lblrspheader_.accept(new LblRspHeaderVisitor<R,A>(), arg);
      p.lblrspbody_.accept(new LblRspBodyVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBJustRspBH p, A arg)
    {
      /* Code For KVDBJustRspBH Goes Here */

      p.lblrspbody_.accept(new LblRspBodyVisitor<R,A>(), arg);
      p.lblrspheader_.accept(new LblRspHeaderVisitor<R,A>(), arg);

      return null;
    }

  }
  public class LblReqHeaderVisitor<R,A> implements LblReqHeader.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblReqHdr p, A arg)
    {
      /* Code For KVDBLblReqHdr Goes Here */

      p.reqheader_.accept(new ReqHeaderVisitor<R,A>(), arg);

      return null;
    }

  }
  public class LblRspHeaderVisitor<R,A> implements LblRspHeader.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblRspHdr p, A arg)
    {
      /* Code For KVDBLblRspHdr Goes Here */

      p.rspheader_.accept(new RspHeaderVisitor<R,A>(), arg);

      return null;
    }

  }
  public class LblReqBodyVisitor<R,A> implements LblReqBody.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblReqBody p, A arg)
    {
      /* Code For KVDBLblReqBody Goes Here */

      p.kvdbrequest_.accept(new KVDBRequestVisitor<R,A>(), arg);

      return null;
    }

  }
  public class LblRspBodyVisitor<R,A> implements LblRspBody.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLblRspBody p, A arg)
    {
      /* Code For KVDBLblRspBody Goes Here */

      p.kvdbresponse_.accept(new KVDBResponseVisitor<R,A>(), arg);

      return null;
    }

  }
  public class ReqHeaderVisitor<R,A> implements ReqHeader.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqHdr p, A arg)
    {
      /* Code For KVDBReqHdr Goes Here */

      p.uri_1.accept(new URIVisitor<R,A>(), arg);
      p.uri_2.accept(new URIVisitor<R,A>(), arg);
      p.uuid_1.accept(new UUIDVisitor<R,A>(), arg);
      p.uuid_2.accept(new UUIDVisitor<R,A>(), arg);
      p.reqjust_.accept(new ReqJustVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqNoHdr p, A arg)
    {
      /* Code For KVDBReqNoHdr Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqURIHdr p, A arg)
    {
      /* Code For KVDBReqURIHdr Goes Here */

      p.uri_.accept(new URIVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqUUIDHdr p, A arg)
    {
      /* Code For KVDBReqUUIDHdr Goes Here */

      p.uuid_.accept(new UUIDVisitor<R,A>(), arg);

      return null;
    }

  }
  public class RspHeaderVisitor<R,A> implements RspHeader.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspHdr p, A arg)
    {
      /* Code For KVDBRspHdr Goes Here */

      p.uri_1.accept(new URIVisitor<R,A>(), arg);
      p.uri_2.accept(new URIVisitor<R,A>(), arg);
      p.uuid_1.accept(new UUIDVisitor<R,A>(), arg);
      p.uuid_2.accept(new UUIDVisitor<R,A>(), arg);
      p.rspjust_.accept(new RspJustVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspNoHdr p, A arg)
    {
      /* Code For KVDBRspNoHdr Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspURIHdr p, A arg)
    {
      /* Code For KVDBRspURIHdr Goes Here */

      p.uri_.accept(new URIVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspUUIDHdr p, A arg)
    {
      /* Code For KVDBRspUUIDHdr Goes Here */

      p.uuid_.accept(new UUIDVisitor<R,A>(), arg);

      return null;
    }

  }
  public class KVDBRequestVisitor<R,A> implements KVDBRequest.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskReq p, A arg)
    {
      /* Code For KVDBAskReq Goes Here */

      p.askreq_.accept(new AskReqVisitor<R,A>(), arg);
      p.askreqpacket_.accept(new AskReqPacketVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellReq p, A arg)
    {
      /* Code For KVDBTellReq Goes Here */

      p.tellreq_.accept(new TellReqVisitor<R,A>(), arg);
      p.tellreqpacket_.accept(new TellReqPacketVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNoReq p, A arg)
    {
      /* Code For KVDBNoReq Goes Here */


      return null;
    }

  }
  public class KVDBResponseVisitor<R,A> implements KVDBResponse.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskRsp p, A arg)
    {
      /* Code For KVDBAskRsp Goes Here */

      p.askrsp_.accept(new AskRspVisitor<R,A>(), arg);
      p.askrsppacket_.accept(new AskRspPacketVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellRsp p, A arg)
    {
      /* Code For KVDBTellRsp Goes Here */

      p.tellrsp_.accept(new TellRspVisitor<R,A>(), arg);
      p.tellrsppacket_.accept(new TellRspPacketVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNoRsp p, A arg)
    {
      /* Code For KVDBNoRsp Goes Here */


      return null;
    }

  }
  public class AskReqPacketVisitor<R,A> implements AskReqPacket.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskReqData p, A arg)
    {
      /* Code For KVDBAskReqData Goes Here */

      p.pattern_.accept(new PatternVisitor<R,A>(), arg);

      return null;
    }

  }
  public class AskRspPacketVisitor<R,A> implements AskRspPacket.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBAskRspData p, A arg)
    {
      /* Code For KVDBAskRspData Goes Here */

      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      p.substitution_.accept(new SubstitutionVisitor<R,A>(), arg);
      p.blob_.accept(new BlobVisitor<R,A>(), arg);

      return null;
    }

  }
  public class TellReqPacketVisitor<R,A> implements TellReqPacket.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellReqData p, A arg)
    {
      /* Code For KVDBTellReqData Goes Here */

      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      p.blob_.accept(new BlobVisitor<R,A>(), arg);

      return null;
    }

  }
  public class TellRspPacketVisitor<R,A> implements TellRspPacket.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBTellRspData p, A arg)
    {
      /* Code For KVDBTellRspData Goes Here */

      p.status_.accept(new StatusVisitor<R,A>(), arg);

      return null;
    }

  }
  public class ReqJustVisitor<R,A> implements ReqJust.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqJustNone p, A arg)
    {
      /* Code For KVDBReqJustNone Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBReqJustSome p, A arg)
    {
      /* Code For KVDBReqJustSome Goes Here */

      p.uuid_.accept(new UUIDVisitor<R,A>(), arg);

      return null;
    }

  }
  public class RspJustVisitor<R,A> implements RspJust.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspJustNone p, A arg)
    {
      /* Code For KVDBRspJustNone Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBRspJustSome p, A arg)
    {
      /* Code For KVDBRspJustSome Goes Here */

      p.uuid_.accept(new UUIDVisitor<R,A>(), arg);

      return null;
    }

  }
  public class AskReqVisitor<R,A> implements AskReq.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBGetReq p, A arg)
    {
      /* Code For KVDBGetReq Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBFetchReq p, A arg)
    {
      /* Code For KVDBFetchReq Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubscribeReq p, A arg)
    {
      /* Code For KVDBSubscribeReq Goes Here */


      return null;
    }

  }
  public class TellReqVisitor<R,A> implements TellReq.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPutReq p, A arg)
    {
      /* Code For KVDBPutReq Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStoreReq p, A arg)
    {
      /* Code For KVDBStoreReq Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPublishReq p, A arg)
    {
      /* Code For KVDBPublishReq Goes Here */


      return null;
    }

  }
  public class AskRspVisitor<R,A> implements AskRsp.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBGetRsp p, A arg)
    {
      /* Code For KVDBGetRsp Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBFetchRsp p, A arg)
    {
      /* Code For KVDBFetchRsp Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubscribeRsp p, A arg)
    {
      /* Code For KVDBSubscribeRsp Goes Here */


      return null;
    }

  }
  public class TellRspVisitor<R,A> implements TellRsp.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPutRsp p, A arg)
    {
      /* Code For KVDBPutRsp Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStoreRsp p, A arg)
    {
      /* Code For KVDBStoreRsp Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBPublishRsp p, A arg)
    {
      /* Code For KVDBPublishRsp Goes Here */


      return null;
    }

  }
  public class StatusVisitor<R,A> implements Status.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusOk p, A arg)
    {
      /* Code For KVDBStatusOk Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusNotOk p, A arg)
    {
      /* Code For KVDBStatusNotOk Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusCode p, A arg)
    {
      /* Code For KVDBStatusCode Goes Here */

      //p.integer_;

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBStatusStr p, A arg)
    {
      /* Code For KVDBStatusStr Goes Here */

      //p.string_;

      return null;
    }

  }
  public class PatternVisitor<R,A> implements Pattern.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QPointed p, A arg)
    {
      /* Code For QPointed Goes Here */

      p.qryterm_.accept(new QryTermVisitor<R,A>(), arg);

      return null;
    }

  }
  public class BlobVisitor<R,A> implements Blob.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QBlob p, A arg)
    {
      /* Code For QBlob Goes Here */

      //p.string_;

      return null;
    }

  }
  public class SubstitutionVisitor<R,A> implements Substitution.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubst p, A arg)
    {
      /* Code For KVDBSubst Goes Here */

      for (SubstPair x : p.listsubstpair_) {
      }

      return null;
    }

  }
  public class SubstPairVisitor<R,A> implements SubstPair.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBSubstPair p, A arg)
    {
      /* Code For KVDBSubstPair Goes Here */

      //p.varuident_;
      p.qryterm_.accept(new QryTermVisitor<R,A>(), arg);

      return null;
    }

  }
  public class QryTermVisitor<R,A> implements QryTerm.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QTerm p, A arg)
    {
      /* Code For QTerm Goes Here */

      //p.string_;
      p.qryarray_.accept(new QryArrayVisitor<R,A>(), arg);

      return null;
    }

  }
  public class QryElemVisitor<R,A> implements QryElem.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QVar p, A arg)
    {
      /* Code For QVar Goes Here */

      //p.varuident_;

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QVal p, A arg)
    {
      /* Code For QVal Goes Here */

      p.qryvalue_.accept(new QryValueVisitor<R,A>(), arg);

      return null;
    }

  }
  public class QryValueVisitor<R,A> implements QryValue.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QAtomic p, A arg)
    {
      /* Code For QAtomic Goes Here */

      p.qrygrndlit_.accept(new QryGrndLitVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QColl p, A arg)
    {
      /* Code For QColl Goes Here */

      p.qryarray_.accept(new QryArrayVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QComp p, A arg)
    {
      /* Code For QComp Goes Here */

      p.qryterm_.accept(new QryTermVisitor<R,A>(), arg);

      return null;
    }

  }
  public class QryArrayVisitor<R,A> implements QryArray.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QArray p, A arg)
    {
      /* Code For QArray Goes Here */

      for (QryElem x : p.listqryelem_) {
      }

      return null;
    }

  }
  public class QryGrndLitVisitor<R,A> implements QryGrndLit.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QStr p, A arg)
    {
      /* Code For QStr Goes Here */

      //p.string_;

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QNum p, A arg)
    {
      /* Code For QNum Goes Here */

      p.qrynum_.accept(new QryNumVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QBool p, A arg)
    {
      /* Code For QBool Goes Here */

      p.qrybool_.accept(new QryBoolVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QNul p, A arg)
    {
      /* Code For QNul Goes Here */


      return null;
    }

  }
  public class QryBoolVisitor<R,A> implements QryBool.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QTru p, A arg)
    {
      /* Code For QTru Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QFal p, A arg)
    {
      /* Code For QFal Goes Here */


      return null;
    }

  }
  public class QryNumVisitor<R,A> implements QryNum.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QInt p, A arg)
    {
      /* Code For QInt Goes Here */

      //p.integer_;

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.QDbl p, A arg)
    {
      /* Code For QDbl Goes Here */

      //p.double_;

      return null;
    }

  }
  public class URIVisitor<R,A> implements URI.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.BasicURI p, A arg)
    {
      /* Code For BasicURI Goes Here */

      p.urischeme_.accept(new URISchemeVisitor<R,A>(), arg);
      p.uripath_.accept(new URIPathVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.NullURI p, A arg)
    {
      /* Code For NullURI Goes Here */


      return null;
    }

  }
  public class URIPathVisitor<R,A> implements URIPath.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.LocatedtedPath p, A arg)
    {
      /* Code For LocatedtedPath Goes Here */

      p.urilocation_.accept(new URILocationVisitor<R,A>(), arg);
      p.urirelativepath_.accept(new URIRelativePathVisitor<R,A>(), arg);

      return null;
    }

  }
  public class URILocationVisitor<R,A> implements URILocation.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URINetLocation p, A arg)
    {
      /* Code For URINetLocation Goes Here */

      p.uriroot_.accept(new URIRootVisitor<R,A>(), arg);
      p.urirsrclocation_.accept(new URIRsrcLocationVisitor<R,A>(), arg);

      return null;
    }

  }
  public class URIRsrcLocationVisitor<R,A> implements URIRsrcLocation.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRsrcPortLoc p, A arg)
    {
      /* Code For URIRsrcPortLoc Goes Here */

      p.netlocation_.accept(new NetLocationVisitor<R,A>(), arg);
      p.port_.accept(new PortVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIRsrcLoc p, A arg)
    {
      /* Code For URIRsrcLoc Goes Here */

      p.netlocation_.accept(new NetLocationVisitor<R,A>(), arg);

      return null;
    }

  }
  public class URIRelativePathVisitor<R,A> implements URIRelativePath.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.SlashPath p, A arg)
    {
      /* Code For SlashPath Goes Here */

      p.uriroot_.accept(new URIRootVisitor<R,A>(), arg);
      for (URIPathElement x : p.listuripathelement_) {
      }

      return null;
    }

  }
  public class URIRootVisitor<R,A> implements URIRoot.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.URIOrigin p, A arg)
    {
      /* Code For URIOrigin Goes Here */


      return null;
    }

  }
  public class NetLocationVisitor<R,A> implements NetLocation.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.DNSAddr p, A arg)
    {
      /* Code For DNSAddr Goes Here */

      for (DNSElement x : p.listdnselement_) {
      }

      return null;
    }

  }
  public class URISchemeVisitor<R,A> implements URIScheme.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomScheme p, A arg)
    {
      /* Code For AtomScheme Goes Here */

      //p.lident_;

      return null;
    }

  }
  public class URIPathElementVisitor<R,A> implements URIPathElement.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomPathElement p, A arg)
    {
      /* Code For AtomPathElement Goes Here */

      //p.lident_;

      return null;
    }

  }
  public class DNSElementVisitor<R,A> implements DNSElement.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomDNSElement p, A arg)
    {
      /* Code For AtomDNSElement Goes Here */

      //p.lident_;

      return null;
    }

  }
  public class PortVisitor<R,A> implements Port.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.AtomPort p, A arg)
    {
      /* Code For AtomPort Goes Here */

      //p.integer_;

      return null;
    }

  }
  public class UUIDVisitor<R,A> implements UUID.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBLUUID p, A arg)
    {
      /* Code For KVDBLUUID Goes Here */

      //p.lident_;

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBUUUID p, A arg)
    {
      /* Code For KVDBUUUID Goes Here */

      //p.uident_;

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.amqpJSONAPI2.Absyn.KVDBNullUUID p, A arg)
    {
      /* Code For KVDBNullUUID Goes Here */


      return null;
    }

  }
}