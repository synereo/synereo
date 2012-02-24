package com.protegra.agentservicesstore

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URMExtensions._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import moniker._
import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.util.continuations._
import scala.xml._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import com.protegra.agentservicesstore.schema.KVDBSerializable

//this junction lets you only get/put fetch/store objects that include the KVDBSerializable trait
//objects still get serialized to xstream <String> so this is really compiler convenience
//for now it still efficient that we serialize our classes using scala to base64 strings and use the StringMGJ
object AgentSerializableTS
  extends AgentTermStoreScope[ String, String, String, KVDBSerializable ]
  with UUIDOps
{
  import CnxnLeafAndBranch._

  type ACTypes = AgentCnxnTypes

  object TheACT extends ACTypes

  override def protoAgentCnxnTypes: ACTypes = TheACT

  type MTTypes = MonadicTermTypes[ String, String, String, KVDBSerializable ]

  object TheMTT extends MTTypes

  override def protoTermTypes: MTTypes = TheMTT

  type DATypes = DistributedAskTypes

  object TheDAT extends DATypes

  override def protoAskTypes: DATypes = TheDAT

  type MsgTypes = DTSMSH[ String, String, String, KVDBSerializable ]
  type CnxnMsgTypes = CnxnDTSMSH[ String, String, String, KVDBSerializable ]

  val protoDreqUUID = getUUID()
  val protoDrspUUID = getUUID()
//
//  val kvdbLabel =
//    new CnxnCtxtLeaf[ String, String, String ](
//      Left(
//        "a"
//      )
//    )

  val kvdbValue = new Object with KVDBSerializable

  object MonadicDMsgs extends MsgTypes
  {

    override def protoDreq: DReq = MDGetRequest(aLabel)

    override def protoDrsp: DRsp = MDGetResponse(aLabel, kvdbValue)

    override def protoJtsreq: JTSReq =
      JustifiedRequest(
        protoDreqUUID,
        new URM("agent", protoDreqUUID.toString, "/invitation", None),
        new URM("agent", protoDreqUUID.toString, "/invitation", None),
        getUUID(),
        protoDreq,
        None
      )

    override def protoJtsrsp: JTSRsp =
      JustifiedResponse(
        protoDreqUUID,
        new URM("agent", protoDrspUUID.toString, "/invitation", None),
        new URM("agent", protoDrspUUID.toString, "/invitation", None),
        getUUID(),
        protoDrsp,
        None
      )

    override def protoJtsreqorrsp: JTSReqOrRsp =
      Left(protoJtsreq)
  }

  override def protoMsgs: MsgTypes = MonadicDMsgs

  object MonadicCnxnDMsgs extends CnxnMsgTypes
  {
    type ACTypes = AgentCnxnTypes
    type MsgTypes =
    DTSMSH[ String, String, String, KVDBSerializable ]

    override def protoMsgs: MsgTypes = MonadicDMsgs

    override def protoAgentCnxnTypes: ACTypes = TheACT

    override def protoCnxnDreq: CnxnDReq =
      CnxnMDGetRequest(acT.protoAgentCnxn, aLabel)

    override def protoCnxnDrsp: CnxnDRsp =
      CnxnMDGetResponse(
        acT.protoAgentCnxn,
        aLabel,
        kvdbValue
      )


    override def protoCnxnJtsreq: CnxnJTSReq =
      JustifiedRequest(
        protoDreqUUID,
        new URM("agent", protoDreqUUID.toString, "/invitation", None),
        new URM("agent", protoDreqUUID.toString, "/invitation", None),
        getUUID(),
        protoCnxnDreq,
        None
      )

    override def protoCnxnJtsrsp: CnxnJTSRsp =
      JustifiedResponse(
        protoDreqUUID,
        new URM("agent", protoDrspUUID.toString, "/invitation", None),
        new URM("agent", protoDrspUUID.toString, "/invitation", None),
        getUUID(),
        protoCnxnDrsp,
        None
      )

    override def protoCnxnJtsreqorrsp: CnxnJTSReqOrRsp =
      Left(protoCnxnJtsreq)
  }

  override def protoCnxnMsgs: CnxnMsgTypes = MonadicCnxnDMsgs

  class PartitionedSerializableMGJ(
    override val name: URM,
    override val acquaintances: Seq[ URM ],
    override val cnxn: Option[ acT.AgentCnxn ]
    ) extends AgentMonadicGeneratorJunction(
    name, acquaintances, cnxn,
    new HashMap[ acT.AgentCnxn, AgentMonadicGeneratorJunction ]()
  )
  {

    class StringXMLDBManifest(
      override val labelToNS: Option[ String => String ],
      override val textToVar: Option[ String => String ],
      override val textToTag: Option[ String => String ]
      )
      extends XMLDBManifest(database)
    {
      override def storeUnitStr: String =
      {
        throw new Exception("use Cnxn-based interface instead")
      }

      override def storeUnitStr[ Src, Label, Trgt ](
        cnxn: Cnxn[ Src, Label, Trgt ]
        ): String =
      {
        cnxn match {
          case agentCnxn: acT.AgentCnxn =>
            agentCnxn.src.getHost + agentCnxn.trgt.getHost
          case _ =>
            throw new Exception("unexpected cnxn type")
        }
      }

      def kvNameSpace: String = "record"

      // BUGBUG -- LGM: Evidence of a problem with this factorization
      override def asCacheValue(
        ltns: String => String,
        ttv: String => String,
        value: Elem
        ): Option[ KVDBSerializable ] =
      {
        tweet(
          "Shouldn't be here!"

        )
        None
      }

      override def asStoreValue(
        rsrc: mTT.Resource
        ): CnxnCtxtLeaf[ String, String, String ] with Factual =
      {
        valueStorageType match {
          case "CnxnCtxtLabel" => {
            tweet(
              "warning: CnxnCtxtLabel method is using XStream"

            )

            val blob = toXQSafeJSONBlob(rsrc)

            new CnxnCtxtLeaf[ String, String, String ](
              Left[ String, String ](
                blob
              )
            )
          }
          case "XStream" => {
            tweet(
              "using XStream method"

            )

            val blob = toXQSafeJSONBlob(rsrc)
            //asXML( rsrc )

            new CnxnCtxtLeaf[ String, String, String ](
              Left[ String, String ](blob)
            )
          }
          case _ => {
            throw new Exception("unexpected value storage type")
          }
        }
      }

      def asCacheValue(
        ccl: CnxnCtxtLabel[ String, String, String ]
        ): KVDBSerializable =
      {
        tweet(
          "converting to cache value"

        )
        //asPatternString( ccl )
        ccl match {
          case CnxnCtxtBranch(
          storeType,
          CnxnCtxtLeaf(Left(rv)) :: Nil
          ) => {
            val unBlob = fromXQSafeJSONBlob(rv)

            unBlob match {
              case rsrc: mTT.Resource => {
                //should be made to an option
                ( getGV(rsrc).getOrElse(null.asInstanceOf[KVDBSerializable]) )
              }
              case _ => null.asInstanceOf[KVDBSerializable]
            }

          }
          case _ => {
            new Object with KVDBSerializable
//            asPatternString(
//              ccl.asInstanceOf[ CnxnCtxtLabel[ String, String, String ] ]
//            )
          }
        }
      }

    }

    def makeSpace(cnxn: acT.AgentCnxn) =
    {
      val symmIdStr =
        cnxn.symmetricIdentityString

      tweet(
        (
          "Symmetric cnxn identity is "
            + symmIdStr
          )
      )

      val nacqs =
        (
          for ( acq <- acquaintances )
          yield {
            acq.withPath(acq.getPath + "/" + symmIdStr)
          }
          );

      val nname = name.withPath(name.getPath + "/" + symmIdStr)

      new PartitionedSerializableMGJ(nname, nacqs, Some(cnxn))
    }

    var _persistenceManifest: Option[ PersistenceManifest ] = None

    def persistenceManifest: Option[ PersistenceManifest ] =
    {
      _persistenceManifest match {
        case None => {
          val sid = Some((s: String) => s)
          val pm =
            Some(
              new StringXMLDBManifest(sid, sid, sid)
            )
          _persistenceManifest = pm
          pm
        }
        case Some(_) => _persistenceManifest
      }
    }
  }

}