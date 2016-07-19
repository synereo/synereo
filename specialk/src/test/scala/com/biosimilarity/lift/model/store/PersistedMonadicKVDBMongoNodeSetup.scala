package com.biosimilarity.lift.model.store

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream}
import java.net.URI

import biz.source_code.base64Coder.Base64Coder
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.lib.moniker.identityConversions._
import com.biosimilarity.lift.model.msg._
import com.mongodb.casbah.Imports._

import scala.collection.mutable
import scala.util.continuations._

object PersistedMonadicKVDBMongoNodeSetup
    extends PersistedMonadicKVDBMongoNodeScope[String, String, String, String]
    with UUIDOps
    with Serializable {

  type MTTypes = MonadicTermTypes[String, String, String, String]
  object TheMTT extends MTTypes with Serializable
  override def protoTermTypes: MTTypes = TheMTT

  type DATypes = DistributedAskTypes
  object TheDAT extends DATypes with Serializable
  override def protoAskTypes: DATypes = TheDAT

  override type MsgTypes     = DTSMSHRsrc
  override type RsrcMsgTypes = DTSMSHRsrc

  type PNReq = PersistedKVDBNodeRequest
  type PNRsp = PersistedKVDBNodeResponse

  @transient val protoDreqUUID = getUUID()
  @transient val protoDrspUUID = getUUID()
  @transient lazy val aLabel   = new CnxnCtxtLeaf[String, String, String](Left("a"))

  val reqUri: URI = new URI("agent", protoDreqUUID.toString, "/invitation", "")
  val rspUri: URI = new URI("agent", protoDrspUUID.toString, "/invitation", "")

  object MonadicDRsrcMsgs extends RsrcMsgTypes with Serializable {
    @transient override def protoDreq: DReq     = MDGetRequest(aLabel)
    @transient override def protoDrsp: DRsp     = MDGetResponse(aLabel, "")
    @transient override def protoJtsreq: JTSReq = JustifiedRequest(protoDreqUUID, reqUri, reqUri, getUUID(), protoDreq, None)
    @transient override def protoJtsrsp: JTSRsp = JustifiedResponse(protoDrspUUID, rspUri, rspUri, getUUID(), protoDrsp, None)
    override def protoJtsreqorrsp: JTSReqOrRsp  = Left(protoJtsreq)
  }

  override def protoMsgs: MsgTypes         = MonadicDRsrcMsgs
  override def protoRsrcMsgs: RsrcMsgTypes = MonadicDRsrcMsgs

  object Being extends PersistenceScope with Serializable {

    override type EMTypes = ExcludedMiddleTypes[mTT.GetRequest, mTT.GetRequest, mTT.Resource]

    object theEMTypes extends ExcludedMiddleTypes[mTT.GetRequest, mTT.GetRequest, mTT.Resource] with Serializable {

      case class PrologSubstitution(soln: mutable.LinkedHashMap[String, CnxnCtxtLabel[String, String, String]])
          extends ((mTT.Resource) => Option[mTT.Resource]) {

        override def apply(rsrc: mTT.Resource) = Some(mTT.RBoundHM(Some(rsrc), Some(soln)))
      }

      override type Substitution = PrologSubstitution
    }

    override def protoEMTypes: EMTypes = theEMTypes

    object PersistedKVDBNodeFactory extends PersistedKVDBNodeFactoryT with Serializable {

      def mkCache[ReqBody <: PNReq, RspBody <: PNRsp](here: URI): PersistedMonadicKVDB[ReqBody, RspBody] = {

        new PersistedMonadicKVDB[ReqBody, RspBody](MURI(here)) with Blobify with AMQPMonikerOps {

          class StringMongoDBManifest(override val storeUnitStr: String,
                                      @transient override val labelToNS: Option[String => String],
                                      @transient override val textToVar: Option[String => String],
                                      @transient override val textToTag: Option[String => String])
              extends MongoDBManifest() {

            override def valueStorageType: String =
              throw new Exception("valueStorageType not overridden in instantiation")

            override def continuationStorageType: String =
              throw new Exception("continuationStorageType not overridden in instantiation")

            override def storeUnitStr[Src, Label, Trgt](cnxn: Cnxn[Src, Label, Trgt]): String =
              cnxn match {
                case CCnxn(s, l, t) => s.toString + l.toString + t.toString
              }

            def kvNameSpace: String  = "record"
            def kvKNameSpace: String = "kRecord"

            def compareNameSpace(ns1: String, ns2: String): Boolean =
              ns1.equals(ns2)

            override def asStoreValue(rsrc: mTT.Resource): CnxnCtxtLeaf[String, String, String] with Factual = {
              val storageDispatch = rsrc match {
                case k: mTT.Continuation => continuationStorageType
                case _                   => valueStorageType
              }
              val blob = storageDispatch match {
                case "Base64" =>
                  val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
                  val oos: ObjectOutputStream     = new ObjectOutputStream(baos)
                  oos.writeObject(rsrc.asInstanceOf[Serializable])
                  oos.close()
                  new String(Base64Coder.encode(baos.toByteArray))
                case "CnxnCtxtLabel" =>
                  toXQSafeJSONBlob(rsrc)
                case "XStream" =>
                  toXQSafeJSONBlob(rsrc)
                case _ =>
                  throw new Exception("unexpected value storage type")
              }
              new CnxnCtxtLeaf[String, String, String](Left[String, String](blob))
            }

            def asCacheValue(ccl: CnxnCtxtLabel[String, String, String]): String = {
              ccl match {
                case CnxnCtxtBranch("string", CnxnCtxtLeaf(Left(rv)) :: Nil) =>
                  val unBlob = fromXQSafeJSONBlob(rv)
                  unBlob match {
                    case TheMTT.Ground(value) => value
                    case rsrc: mTT.Resource   => getGV(rsrc).getOrElse("WHOOPS!")
                  }
                case _ =>
                  throw new Exception(s"unexpected value form: $ccl")
              }
            }

            override def asResource(key: mTT.GetRequest, value: DBObject): emT.PlaceInstance = {
              val ltns = labelToNS.getOrElse(throw new Exception("must have labelToNS to convert mongo object"))
              val ttv  = textToVar.getOrElse(throw new Exception("must have textToVar to convert mongo object"))
              val ttt  = textToTag.getOrElse(throw new Exception("must have textToTag to convert mongo object"))
              val computedRslt = CnxnMongoObjectifier().fromMongoObject(value)(ltns, ttv, ttt) match {
                case CnxnCtxtBranch(ns, CnxnCtxtBranch(kNs, k :: Nil) :: CnxnCtxtBranch(vNs, v :: Nil) :: Nil) =>
                  val matchRslt = matchMap(key, k)
                  matchRslt match {
                    case Some(soln) =>
                      if (compareNameSpace(ns, kvNameSpace)) {
                        val cacheValueRslt = asCacheValue(new CnxnCtxtBranch[String, String, String]("string", v :: Nil))
                        val groundWrapper  = mTT.Ground(cacheValueRslt)
                        val boundHMWrapper = mTT.RBoundHM(Some(groundWrapper), Some(soln))
                        val boundWrapper   = mTT.asRBoundAList(boundHMWrapper)
                        emT.PlaceInstance(k,
                                          Left[mTT.Resource, List[Option[mTT.Resource] => Unit @suspendable]](boundWrapper),
                                          theEMTypes.PrologSubstitution(soln).asInstanceOf[emT.Substitution])
                      } else {
                        if (compareNameSpace(ns, kvKNameSpace)) {
                          val mTT.Continuation(ks) = asCacheK(new CnxnCtxtBranch[String, String, String]("string", v :: Nil))
                          emT.PlaceInstance(k,
                                            Right[mTT.Resource, List[Option[mTT.Resource] => Unit @suspendable]](ks),
                                            theEMTypes.PrologSubstitution(soln).asInstanceOf[emT.Substitution])
                        } else {
                          throw new Exception(s"unexpected namespace: $ns")
                        }
                      }
                    case None =>
                      throw new UnificationQueryFilter(key, k, value)
                  }
                case _ =>
                  throw new Exception(s"unexpected record format: $value")
              }
              computedRslt
            }
          }

          override def asCacheK(ccl: CnxnCtxtLabel[String, String, String]): Option[mTT.Continuation] = {
            ccl match {
              case CnxnCtxtBranch("string", CnxnCtxtLeaf(Left(rv)) :: Nil) =>
                val unBlob = continuationStorageType match {
                  case "CnxnCtxtLabel" => fromXQSafeJSONBlob(rv)
                  case "XStream"       => fromXQSafeJSONBlob(rv)
                  case "Base64" =>
                    val data: Array[Byte]               = Base64Coder.decode(rv)
                    val ois: DefensiveObjectInputStream = new DefensiveObjectInputStream(new ByteArrayInputStream(data))
                    val o: java.lang.Object             = ois.readObject()
                    ois.close()
                    o
                }
                unBlob match {
                  case k: mTT.Resource =>
                    Some(k.asInstanceOf[mTT.Continuation])
                  case _ =>
                    throw new Exception(s"ill-formatted continuation stack blob: $unBlob")
                }
              case _ =>
                throw new Exception(s"ill-formatted continuation stack leaf: $ccl")
            }
          }

          override def asCacheK(ltns: String => String, ttv: String => String, value: DBObject): Option[mTT.Continuation] =
            throw new Exception("shouldn't be calling this version of asCacheK")

          override def persistenceManifest: Option[PersistenceManifest] = {
            val sid  = Some((s: String) => recoverFieldName(s))
            val kvdb = this
            Some(new StringMongoDBManifest(dfStoreUnitStr, sid, sid, sid) {
              override def valueStorageType: String        = kvdb.valueStorageType
              override def continuationStorageType: String = kvdb.continuationStorageType
            })
          }

          def dfStoreUnitStr: String = mnkrExchange(name)
        }
      }

      def ptToPt[ReqBody <: PNReq, RspBody <: PNRsp](here: URI, there: URI): PersistedMonadicKVDBNode[ReqBody, RspBody] = {
        val node = PersistedMonadicKVDBNode[ReqBody, RspBody](mkCache(MURI(here)), List(MURI(there)))
        spawn { node.dispatchDMsgs() }
        node
      }

      def ptToMany[ReqBody <: PNReq, RspBody <: PNRsp](here: URI, there: List[URI]): PersistedMonadicKVDBNode[ReqBody, RspBody] = {
        val node = PersistedMonadicKVDBNode[ReqBody, RspBody](mkCache(MURI(here)), there.map(MURI(_)))
        spawn { node.dispatchDMsgs() }
        node
      }

      def loopBack[ReqBody <: PNReq, RspBody <: PNRsp](here: URI): PersistedMonadicKVDBNode[ReqBody, RspBody] = {
        val exchange = uriExchange(here)
        val hereNow =
          new URI(here.getScheme, here.getUserInfo, here.getHost, here.getPort, "/" + exchange + "Local", here.getQuery, here.getFragment)
        val thereNow =
          new URI(here.getScheme, here.getUserInfo, here.getHost, here.getPort, "/" + exchange + "Remote", here.getQuery, here.getFragment)
        val node = PersistedMonadicKVDBNode[ReqBody, RspBody](mkCache(MURI(hereNow)), List(MURI(thereNow)))
        spawn { node.dispatchDMsgs() }
        node
      }
    }
  }

  type BUMPKIN[A <: PNReq, B <: PNRsp] = Being.PersistedMonadicKVDBNode[A, B]

  def setup[ReqBody <: PNReq, RspBody <: PNRsp](localHost: String, localPort: Int, remoteHost: String, remotePort: Int)(
      implicit returnTwist: Boolean = false): Either[BUMPKIN[ReqBody, RspBody], (BUMPKIN[ReqBody, RspBody], BUMPKIN[ReqBody, RspBody])] = {
    val (localExchange, remoteExchange) =
      if (localHost.equals(remoteHost) && (localPort == remotePort))
        ("/mongoUseCaseProtocolLocal", "/mongoUseCaseProtocolRemote")
      else
        ("/mongoUseCaseProtocol", "/mongoUseCaseProtocol")
    if (returnTwist)
      Right[BUMPKIN[ReqBody, RspBody], (BUMPKIN[ReqBody, RspBody], BUMPKIN[ReqBody, RspBody])](
        Being.PersistedKVDBNodeFactory.ptToPt[ReqBody, RspBody](
          new URI("agent", null, localHost, localPort, localExchange, null, null),
          new URI("agent", null, remoteHost, remotePort, remoteExchange, null, null)),
        Being.PersistedKVDBNodeFactory.ptToPt[ReqBody, RspBody](new URI("agent", null, remoteHost, remotePort, remoteExchange, null, null),
                                                                new URI("agent", null, localHost, localPort, localExchange, null, null)))
    else
      Left[BUMPKIN[ReqBody, RspBody], (BUMPKIN[ReqBody, RspBody], BUMPKIN[ReqBody, RspBody])](
        Being.PersistedKVDBNodeFactory.ptToPt[ReqBody, RspBody](
          new URI("agent", null, localHost, localPort, localExchange, null, null),
          new URI("agent", null, remoteHost, remotePort, remoteExchange, null, null)))
  }
}
