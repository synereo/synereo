// -*- mode: Scala;-*- 
// Filename:    DSLCommLink.scala<2> 
// Authors:     lgm                                                    
// Creation:    Mon Apr 22 06:04:10 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.xml._
//import com.biosimilarity.lift.model.store.mongo._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._
import net.liftweb.amqp._

import scala.util.continuations._ 
import scala.concurrent.{Channel => Chan, _}
//import scala.concurrent.cpsops._
import com.biosimilarity.lift.lib.concurrent._
import com.biosimilarity.lift.lib.concurrent.cpsops._
import scala.xml._
import scala.collection.mutable.Map
import scala.collection.mutable.MapProxy
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.MutableList

//import com.rabbitmq.client._

import org.prolog4j._

import com.mongodb.casbah.Imports._

//import org.json4s._
//import org.json4s.jackson.JsonMethods._
//import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import biz.source_code.base64Coder.Base64Coder

import javax.xml.transform.OutputKeys

import java.util.UUID
import java.net.URI
import java.util.Properties
import java.io.File
import java.io.FileInputStream
import java.io.OutputStreamWriter
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayOutputStream
import com.typesafe.config.{Config, ConfigObject, ConfigValue}
import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

object DSLCommLink
       extends PersistedMonadicKVDBMongoNodeScope[String,String,String,ConcreteHL.HLExpr]
       with UUIDOps
  with Serializable
{
  import SpecialKURIDefaults._
  import identityConversions._
  
  type MTTypes = MonadicTermTypes[String,String,String,ConcreteHL.HLExpr]
  object TheMTT extends MTTypes with Serializable
  override def protoTermTypes : MTTypes = TheMTT
  
  type DATypes = DistributedAskTypes
  object TheDAT extends DATypes with Serializable
  override def protoAskTypes : DATypes = TheDAT
  
  override type MsgTypes = DTSMSHRsrc   
  override type RsrcMsgTypes = DTSMSHRsrc   
  
  @transient
  val protoDreqUUID = getUUID()
  @transient
  val protoDrspUUID = getUUID()    
  
  @transient
  lazy val aLabel = new CnxnCtxtLeaf[String,String,String]( Left( "a" ) )
  
  object MonadicDRsrcMsgs extends RsrcMsgTypes with Serializable {
    
    @transient
    override def protoDreq : DReq = MDGetRequest( aLabel )
    @transient
    override def protoDrsp : DRsp = MDGetResponse( aLabel, ConcreteHL.Bottom )
    @transient
    override def protoJtsreq : JTSReq =
      JustifiedRequest(
        protoDreqUUID,
        new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
        new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
        getUUID(),
        protoDreq,
        None
      )
    @transient
    override def protoJtsrsp : JTSRsp = 
      JustifiedResponse(
        protoDreqUUID,
        new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
        new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
        getUUID(),
        protoDrsp,
        None
      )
    override def protoJtsreqorrsp : JTSReqOrRsp =
      Left( protoJtsreq )
  }
  
  override def protoMsgs : MsgTypes = MonadicDRsrcMsgs
  override def protoRsrcMsgs : RsrcMsgTypes = MonadicDRsrcMsgs
  
  object Being extends PersistenceScope with Serializable {      
    override type EMTypes = ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]
    object theEMTypes extends ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]
       with Serializable
    {
      case class PrologSubstitution( soln : LinkedHashMap[String,CnxnCtxtLabel[String,String,String]] )
           extends Function1[mTT.Resource,Option[mTT.Resource]] {
             override def apply( rsrc : mTT.Resource ) = {
               Some( mTT.RBoundHM( Some( rsrc ), Some( soln ) ) )
             }
           }
      override type Substitution = PrologSubstitution   
    }      
    
    override def protoEMTypes : EMTypes =
      theEMTypes
    
    object PersistedKVDBNodeFactory extends PersistedKVDBNodeFactoryT with Serializable {         
      def mkCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( here : URI ) : PersistedMonadicKVDB[ReqBody,RspBody] = {
        new PersistedMonadicKVDB[ReqBody, RspBody]( MURI( here ) ) with Blobify with AMQPMonikerOps {           
          override def toXQSafeJSONBlob( x : java.lang.Object ) : String = {
            new XStream( new JettisonMappedXmlDriver() ).toXML( x )
          }
          override def fromXQSafeJSONBlob( blob : String ) : java.lang.Object = {              
            new XStream( new JettisonMappedXmlDriver() ).fromXML( blob )
          }

          override def assertComposes(indirect: CnxnCtxtLabel[String, String, String] with Factual,
                                      direct: CnxnCtxtLabel[String, String, String] with Factual): Boolean =
            (indirect, direct) match {
              case (CnxnCtxtBranch(ns1, _ :: CnxnCtxtBranch(vNs1, fk1 :: Nil) :: Nil),
              CnxnCtxtBranch(ns2, CnxnCtxtBranch(kNs2, k2 :: Nil) :: _ :: Nil)) =>
                val flatKey1: String = fk1 match {
                  case CnxnCtxtLeaf(Left(v)) =>
                    fromXQSafeJSONBlob(v) match {
                      case TheMTT.Ground(ConcreteHL.FlatKeyBouncer(CnxnCtxtLeaf(Left(theRealFlatKey)))) => theRealFlatKey
                    }
                }
                val CnxnCtxtBranch(_, CnxnCtxtLeaf(Left(flatKey2)) :: Nil) = k2
                flatKey1 == flatKey2
              case _ =>
                throw new Exception(s"""Records don't have correct shape:
                                        |indirect: $indirect
                                        |direct: $direct""".stripMargin)
            }

          class StringMongoDBManifest(
            override val storeUnitStr : String,
            @transient override val labelToNS : Option[String => String],
            @transient override val textToVar : Option[String => String],
            @transient override val textToTag : Option[String => String],
            @transient override val textToValue: Option[String => ConcreteHL.HLExpr] = Some { (s: String) => ConcreteHL.FlatKeyBouncer(new CnxnCtxtLeaf[String, String, String](Left(s))) }
          )
          extends MongoDBManifest( ) {
            override def valueStorageType : String = {
              throw new Exception( "valueStorageType not overriden in instantiation" )
            }
            override def continuationStorageType : String = {
              throw new Exception( "continuationStorageType not overriden in instantiation" )
            }
            
            override def storeUnitStr[Src,Label,Trgt]( cnxn : Cnxn[Src,Label,Trgt] ) : String = {     
              cnxn match {
                case CCnxn( s, l, t ) => s.toString + l.toString + t.toString
              }     
            }   
            
            def kvNameSpace : String = "record"
            def kvKNameSpace : String = "kRecord"
            
            def compareNameSpace( ns1 : String, ns2 : String ) : Boolean = {
              ns1.equals( ns2 )
            }
            
            override def asStoreValue(
              rsrc : mTT.Resource
            ) : CnxnCtxtLeaf[String,String,String] with Factual = {
              BasicLogService.tweet(
                "In asStoreValue on " + this + " for resource: " + rsrc
              )
              val storageDispatch = 
                rsrc match {
                  case k : mTT.Continuation => {
                    BasicLogService.tweet(
                      "Resource " + rsrc + " is a continuation"
                    )
                    continuationStorageType
                  }
                  case _ => {
                    BasicLogService.tweet(
                      "Resource " + rsrc + " is a value"
                    )
                    valueStorageType
                  }
                };
              
              BasicLogService.tweet(
                "storageDispatch: " + storageDispatch
              )
              
              val blob =
                storageDispatch match {
                  case "Base64" => {
                    val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
                    val oos : ObjectOutputStream = new ObjectOutputStream( baos )
                    oos.writeObject( rsrc.asInstanceOf[Serializable] )
                    oos.close()
                    new String( Base64Coder.encode( baos.toByteArray() ) )
                  }
                  case "CnxnCtxtLabel" => {
                    BasicLogService.tweet(
                      "warning: CnxnCtxtLabel method is using XStream"
                    )
                    toXQSafeJSONBlob( rsrc )                              
                  }
                  case "XStream" => {
                    BasicLogService.tweet(
                      "using XStream method"
                    )
                    
                    toXQSafeJSONBlob( rsrc )
                  }
                  case _ => {
                    throw new Exception( "unexpected value storage type" )
                  }
                }
              new CnxnCtxtLeaf[String,String,String](
                Left[String,String]( blob )
              )
            }

            def asCacheValue(ccl: CnxnCtxtLabel[String, String, String]): ConcreteHL.HLExpr = ccl match {
              case CnxnCtxtBranch("string", CnxnCtxtLeaf(Left(rv)) :: Nil) =>
                fromXQSafeJSONBlob(rv) match {
                  case rsrc: mTT.Resource => getGV(rsrc).getOrElse(ConcreteHL.Bottom)
                }
              case CnxnCtxtLeaf(Left(rv)) =>
                fromXQSafeJSONBlob(rv) match {
                  case rsrc: mTT.Resource => getGV(rsrc).getOrElse(ConcreteHL.Bottom)
                }
              case _ =>
                throw new Exception(s"unexpected value form: $ccl")
            }

            override def asStoreRecord(key: mTT.GetRequest, value: mTT.Resource): CnxnCtxtLabel[String, String, String] with Factual =
              key match {
                case CnxnCtxtBranch(ns, CnxnCtxtBranch(kNs, k :: Nil) :: CnxnCtxtBranch(vNs, fkbs :: Nil) :: Nil) => {
                  val ttt  = textToTag.getOrElse(throw new Exception("must have textToTag to convert flatKey: " + fkbs))
                  val ltns = labelToNS.getOrElse(throw new Exception("must have labelToNS to convert flatKey: " + fkbs))
                  val ConcreteHL.FlatKeyBouncer(CnxnCtxtLeaf(Left(fkS))) = asCacheValue(fkbs)
                  val flatKey: String = ttt(fkS)
                  asStoreEntry(asStoreKey(new CnxnCtxtBranch[String, String, String](ltns(fkS), (new CnxnCtxtLeaf[String, String, String](Left((flatKey)))) :: Nil)), value)(kvNameSpace)
                }
                case _ =>
                  throw new Exception(s"""we should never get here! key: $key , value : $value""")
              }

            override def asStoreKRecord(key: mTT.GetRequest, value: mTT.Resource): CnxnCtxtLabel[String, String, String] with Factual =
              key match {
                case CnxnCtxtBranch(ns, CnxnCtxtBranch(kNs, k :: Nil) :: CnxnCtxtBranch(vNs, fkbs :: Nil) :: Nil) => {
                  val ttt  = textToTag.getOrElse(throw new Exception("must have textToTag to convert flatKey: " + fkbs))
                  val ltns = labelToNS.getOrElse(throw new Exception("must have labelToNS to convert flatKey: " + fkbs))
                  val ConcreteHL.FlatKeyBouncer(CnxnCtxtLeaf(Left(fkS))) = asCacheValue(fkbs)
                  val flatKey: String = ttt(fkS)
                  asStoreEntry(asStoreKey(new CnxnCtxtBranch[String, String, String](ltns(fkS), (new CnxnCtxtLeaf[String, String, String](Left((flatKey)))) :: Nil)), value)(kvKNameSpace)
                }
                case _ =>
                  throw new Exception(s"""we should never get here! key: $key , value : $value""")
              }

            override def asIndirection(key: mTT.GetRequest, value: DBObject): mTT.GetRequest = {
              val ltns = labelToNS.getOrElse(throw new Exception("must have labelToNS to convert mongo object"))
              val ttv  = textToVar.getOrElse(throw new Exception("must have textToVar to convert mongo object"))
              val ttt  = textToTag.getOrElse(throw new Exception("must have textToTag to convert mongo object"))
              CnxnMongoObjectifier().fromMongoObject(value)(ltns, ttv, ttt) match {
                case CnxnCtxtBranch(ns, CnxnCtxtBranch(kNs, k :: Nil) :: CnxnCtxtBranch(vNs, fk :: Nil) :: Nil) =>
                  val matchRslt = matchMap(key, k)
                  matchRslt match {
                    case Some(_) =>
                      fk match {
                        case CnxnCtxtLeaf(Left(v)) =>
                          val unblob: String = fromXQSafeJSONBlob(v) match {
                            case TheMTT.Ground(ConcreteHL.FlatKeyBouncer(CnxnCtxtLeaf(Left(theRealFlatKey)))) => theRealFlatKey
                          }
                          new CnxnCtxtBranch(ltns(unblob), new CnxnCtxtLeaf[String, String, String](Left(unblob)) :: Nil)
                      }
                    case None =>
                      throw new UnificationQueryFilter(key, k, value)
                  }
                case xFactor =>
                  // Should never get here because it is
                  // unreasonable to have retrieved a DBObject
                  // with the key if the key is malformed
                  throw new Exception("unexpected record structure:	" + xFactor)
              }
            }

            override def isIndirectionKey(functor: String, flatKeyCandidate: String): Boolean =
              nameSpaceToString(functor) != tagToString(flatKeyCandidate)

            override def isIndirection(rcrd: CnxnCtxtBranch[String, String, String]): Boolean = rcrd match {
              case CnxnCtxtBranch(ns, CnxnCtxtBranch(kNs, k :: Nil) :: CnxnCtxtBranch(vNs, fk :: Nil) :: Nil) =>
                k match {
                  case CnxnCtxtBranch(functor, CnxnCtxtLeaf(Left(flatKeyCandidate)) :: Nil) =>
                    isIndirectionKey(functor, flatKeyCandidate)
                  case _ =>
                    true
                }
              case _ =>
                throw new Exception(s"unexpected krecord: $rcrd")
            }

            override def asResource(key: mTT.GetRequest, value: DBObject): emT.PlaceInstance = {
              val ltns = labelToNS.getOrElse(throw new Exception("must have labelToNS to convert mongo object"))
              val ttv  = textToVar.getOrElse(throw new Exception("must have textToVar to convert mongo object"))
              val ttt  = textToTag.getOrElse(throw new Exception("must have textToTag to convert mongo object"))
              CnxnMongoObjectifier().fromMongoObject(value)(ltns, ttv, ttt) match {
                case rcrd @ CnxnCtxtBranch(ns, CnxnCtxtBranch(kNs, k :: Nil) :: CnxnCtxtBranch(vNs, v :: Nil) :: Nil) =>
                  matchMap(key, k) match {
                    case Some(soln) =>
                      if (compareNameSpace(ns, kvNameSpace)) {
                        val cacheValueRslt = asCacheValue(new CnxnCtxtBranch[String, String, String]("string", v :: Nil))
                        val groundWrapper = mTT.Ground(cacheValueRslt)
                        val boundHMWrapper = mTT.RBoundHM(Some(groundWrapper), Some(soln))
                        val boundWrapper = mTT.asRBoundAList(boundHMWrapper)
                        emT.PlaceInstance(k,
                          Left[mTT.Resource, List[Option[mTT.Resource] => Unit@suspendable]](boundWrapper),
                          theEMTypes.PrologSubstitution(soln).asInstanceOf[emT.Substitution])
                      } else if ((compareNameSpace(ns, kvKNameSpace)) && isIndirection(rcrd)) {
                        val cacheValueRslt = asCacheValue(new CnxnCtxtBranch[String,String,String]("string", v :: Nil))
                        val groundWrapper = mTT.Ground(cacheValueRslt)
                        val boundHMWrapper = mTT.RBoundHM(Some( groundWrapper ), Some(soln))
                        val boundWrapper = mTT.asRBoundAList(boundHMWrapper)
                        val finalRslt = emT.PlaceInstance(k, Left[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]](boundWrapper), theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution])
                        finalRslt
                      } else if (compareNameSpace(ns, kvKNameSpace)) {
                        val mTT.Continuation(ks) = asCacheK(new CnxnCtxtBranch[String, String, String]("string", v :: Nil))
                        emT.PlaceInstance(k,
                          Right[mTT.Resource, List[Option[mTT.Resource] => Unit@suspendable]](ks),
                          theEMTypes.PrologSubstitution(soln).asInstanceOf[emT.Substitution])
                      } else {
                        throw new Exception(s"unexpected namespace: $ns")
                      }
                    case None =>
                      throw new UnificationQueryFilter(key, k, value)
                  }
                case _ =>
                  throw new Exception(s"unexpected record format: $value")
              }
            }
          }

          override def asCacheK(
            ccl : CnxnCtxtLabel[String,String,String]
          ) : Option[mTT.Continuation] = {
            BasicLogService.tweet(
              "converting to cache continuation stack" + ccl
            )
            ccl match {
              case CnxnCtxtBranch(
                "string",
                CnxnCtxtLeaf( Left( rv ) ) :: Nil
              ) => {
                val unBlob =
                  continuationStorageType match {
                    case "CnxnCtxtLabel" => {
                      // BasicLogService.tweet(
                      //                      "warning: CnxnCtxtLabel method is using XStream"
                      //                    )
                      fromXQSafeJSONBlob( rv )
                    }
                    case "XStream" => {
                      fromXQSafeJSONBlob( rv )
                    }
                    case "Base64" => {
                      val data : Array[Byte] = Base64Coder.decode( rv )
//                       val ois : ObjectInputStream =
//                         new ObjectInputStream( new ByteArrayInputStream(  data ) )
                      val ois : DefensiveObjectInputStream =
		        new DefensiveObjectInputStream( new ByteArrayInputStream(  data ) )
                      val o : java.lang.Object = ois.readObject();
                      ois.close()
                      o
                    }
                  }
                
                unBlob match {
                  case k : mTT.Resource => {
                    Some( k.asInstanceOf[mTT.Continuation] )
                  }
                  case _ => {
                    throw new Exception(
                      (
                        ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                        + "ill-formatted continuation stack blob : " + rv
                        + "\n" 
                        + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                        + "\n"
                        + "unBlob : " + unBlob
                        + "\n"
                        + "unBlob type : " + unBlob
                        + "\n"
                        + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                      )
                    )
                  }
                }
              }
              case _ => {
                throw new Exception( "ill-formatted continuation stack leaf: " + ccl )
              }
            }
          }
          
          override def asCacheK(
            ltns : String => String,
            ttv : String => String,
            value : DBObject
          ) : Option[mTT.Continuation] = {
            throw new Exception( "shouldn't be calling this version of asCacheK" )
          }
          override def persistenceManifest : Option[PersistenceManifest] = {
            val sid = Some( ( s : String ) => recoverFieldName( s ) )
            val kvdb = this
            Some(
              new StringMongoDBManifest(dfStoreUnitStr, sid, sid, sid) {
                override def valueStorageType : String = {
                  kvdb.valueStorageType
                }
                override def continuationStorageType : String = {
                  kvdb.continuationStorageType
                }
              }
            )
          }
          def dfStoreUnitStr : String = mnkrExchange( name )
        }
      }
      def ptToPt[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( here : URI, there : URI ) : PersistedMonadicKVDBNode[ReqBody,RspBody] = {
        val node =
          PersistedMonadicKVDBNode[ReqBody,RspBody](
            mkCache( MURI( here ) ),
            List( MURI( there ) )
          )
        spawn { node.dispatchDMsgs() }
        node
      }
      def ptToMany[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( here : URI, there : List[URI] ) : PersistedMonadicKVDBNode[ReqBody,RspBody] = {
        val node =
          PersistedMonadicKVDBNode[ReqBody,RspBody](
            mkCache( MURI( here ) ),
            there.map( MURI( _ ) )
          )
        spawn { node.dispatchDMsgs() }
        node
      }
      def loopBack[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( here : URI ) : PersistedMonadicKVDBNode[ReqBody,RspBody] = {
        val exchange = uriExchange( here )
        val hereNow =
          new URI(
            here.getScheme,
            here.getUserInfo,
            here.getHost,
            here.getPort,
            "/" + exchange + "Local",
            here.getQuery,
            here.getFragment
          )
        val thereNow =
          new URI(
            here.getScheme,
            here.getUserInfo,
            here.getHost,
            here.getPort,
            "/" + exchange + "Remote",
            here.getQuery,
            here.getFragment
          )         
        
        val node =
          PersistedMonadicKVDBNode[ReqBody, RspBody](
            mkCache( MURI( hereNow ) ),
            List( MURI( thereNow ) )
          )
        spawn { node.dispatchDMsgs() }
        node
      }
    }
  }
}

trait DSLCommLinkConfiguration {
  self : EvalConfig =>
  def clientHostName() : String = {
    try {
      evalConfig().getString( "DSLCommLinkClientHost" )
    }
    catch {
      case e : Throwable => "localhost" 
    }
  }
  def clientPort() : Int = {
    try {
      evalConfig().getInt( "DSLCommLinkClientPort" )
    }
    catch {
      case e : Throwable => 5672
    }
  }
  def clientHostsNPorts() : List[(String,Int)] = {
    try {
      val configObject: List[String] = evalConfig().getStringList("DSLCommLinkClientHosts").toList
      configObject.map { (s: String) =>
        val uri = new URI("unused://" + s)
        // TODO: find another place where Rabbit defaults are defined use them
        val host: String = Option(uri.getHost).getOrElse("localhost")
        val port: Int    = {
          val u = uri.getPort
          if (u == -1) 5672 else u
        }
        (host, port)
      }
    }
    catch {
      case _: Throwable => List[(String, Int)](("localhost", 5672))
    }
  }
  def serverHostName() : String = {
    try {
      evalConfig().getString( "DSLCommLinkServerHost" )
    }
    catch {
      case e : Throwable => "localhost"
    }
  }
  def serverPort() : Int = {
    try {
      evalConfig().getInt( "DSLCommLinkServerPort" )
    }
    catch {
      case e : Throwable => 5672
    }
  }
}

object DSLCommLinkCtor extends EvalConfig
with DSLCommLinkConfiguration
with Serializable {
  import DSLCommLink._
  import Being._
  import PersistedKVDBNodeFactory._

  type EvaluationRequestChannel[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse] =
    Being.PersistedMonadicKVDBNode[ReqBody,RspBody]
  type StdEvaluationRequestChannel = EvaluationRequestChannel[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]

  object ExchangeLabels extends CnxnString[String,String,String] {
    def evalRequestLabel(
      majorVersion : String = "0", minorVersion : String = "1"
    )(
      sessionId : Either[String,String]
    ) = {
      // BUGBUG : lgm -- find a java regex pattern that will pickout
      // prolog variables
      val sessionTerm =
        sessionId match {
          case Left( sessIdStr ) => {
            "\"" + sessIdStr + "\""
          }
          case Right( sessIdVar ) => {
            sessIdVar
          }
        }
      fromTermString(
        "evalRequestLabel( majorVersion( \""
        + majorVersion
        + "\" ), minorVersion( \""
        + minorVersion
        + "\"), sessionId( "
        + sessionTerm
        + " ) )"
      )
    }
    def evalResponseLabel(
      majorVersion : String = "0", minorVersion : String = "1"
    )(
      sessionId : Either[String,String]
    ) = {
      val sessionTerm =
        sessionId match {
          case Left( sessIdStr ) => {
            "\"" + sessIdStr + "\""
          }
          case Right( sessIdVar ) => {
            sessIdVar
          }
        }
      fromTermString(
        "evalResponseLabel( majorVersion( \""
        + majorVersion
        + "\" ), minorVersion( \""
        + minorVersion
        + "\"), sessionId( "
        + sessionTerm
        + " ) )"
      )
    }
    def adminRequestLabel(
      majorVersion : String = "0", minorVersion : String = "1"
    )(
      sessionId : Either[String,String]
    ) = {
      val sessionTerm =
        sessionId match {
          case Left( sessIdStr ) => {
            "\"" + sessIdStr + "\""
          }
          case Right( sessIdVar ) => {
            sessIdVar
          }
        }
      fromTermString(
        "adminRequestLabel( majorVersion( \""
        + majorVersion
        + "\" ), minorVersion( \""
        + minorVersion
        + "\"), sessionId( "
        + sessionTerm
        + " ) )"
      )
    }
    def adminResponseLabel(
      majorVersion : String = "0", minorVersion : String = "1"
    )(
      sessionId : Either[String,String]
    ) = {
      val sessionTerm =
        sessionId match {
          case Left( sessIdStr ) => {
            "\"" + sessIdStr + "\""
          }
          case Right( sessIdVar ) => {
            sessIdVar
          }
        }
      fromTermString(
        "adminResponseLabel( majorVersion( \""
        + majorVersion
        + "\" ), minorVersion( \""
        + minorVersion
        + "\"), sessionId( "
        + sessionTerm
        + " ) )"
      )
    }
  }

/* -------------------------------------------------------------------------- *
 * Basic ctor is the setup method
 * -------------------------------------------------------------------------- */

  def setup[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
    localHost : String, localPort : Int,
    remoteHost : String, remotePort : Int
  )(
    returnTwist : Boolean = false,
    flip : Boolean = false
  ) : Either[EvaluationRequestChannel[ReqBody,RspBody],(EvaluationRequestChannel[ReqBody,RspBody],EvaluationRequestChannel[ReqBody,RspBody])] = {
    val ( localExchange, remoteExchange ) =
      if ( localHost.equals( remoteHost ) && ( localPort == remotePort ) ) {
        ( "/DSLExecProtocolLocal", "/DSLExecProtocolRemote" )
      }
      else {
        ( "/DSLExecProtocol", "/DSLExecProtocol" )
      }

    if ( returnTwist ) {
      Right[EvaluationRequestChannel[ReqBody,RspBody],(EvaluationRequestChannel[ReqBody,RspBody],EvaluationRequestChannel[ReqBody,RspBody])](
        (
          ptToPt[ReqBody, RspBody](
            new URI( "agent", null, localHost, localPort, localExchange, null, null ),
            new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
          ),
          ptToPt[ReqBody, RspBody](
            new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null ),
            new URI( "agent", null, localHost, localPort, localExchange, null, null )
          )
        )
      )
    }
    else {
      Left[EvaluationRequestChannel[ReqBody,RspBody],(EvaluationRequestChannel[ReqBody,RspBody],EvaluationRequestChannel[ReqBody,RspBody])](
        if ( flip ) {
          ptToPt(
            new URI( "agent", null, localHost, localPort, localExchange, null, null ),
            new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
          )
        } else {
          ptToPt(
            new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null ),
            new URI( "agent", null, localHost, localPort, localExchange, null, null )
          )
        }
      )
    }
  }

  def setup[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
    localHost : String, localPort : Int,
    remoteHostsNPorts : List[(String,Int)]
  )(
    returnTwist : Boolean = false,
    flip : Boolean = false
  ) : List[Either[EvaluationRequestChannel[ReqBody,RspBody],(EvaluationRequestChannel[ReqBody,RspBody],EvaluationRequestChannel[ReqBody,RspBody])]] = {
    for( ( remoteHost, remotePort ) <- remoteHostsNPorts )
    yield {
      setup[ReqBody,RspBody](
	localHost, localPort, remoteHost, remotePort
      )( returnTwist, flip )
    }
  }

/* -------------------------------------------------------------------------- *
 * To get a single link get the left hand value of the setup return
 * -------------------------------------------------------------------------- */

  def oneLink[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
    localHost : String = serverHostName, localPort : Int = serverPort,
    remoteHost : String = clientHostName, remotePort : Int = clientPort
  )( flip : Boolean = false ) : EvaluationRequestChannel[ReqBody,RspBody] = {
    val Left( client ) =
      setup[ReqBody,RspBody]( localHost, localPort, remoteHost, remotePort )( false, flip )
    client
  }

  def link[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
    localHost : String = serverHostName, localPort : Int = serverPort,
    remoteHostsNPorts : List[(String,Int)] = clientHostsNPorts
  )( flip : Boolean = false ) : List[EvaluationRequestChannel[ReqBody,RspBody]] = {
    for( ( remoteHost, remotePort ) <- remoteHostsNPorts )
    yield {
      oneLink[ReqBody,RspBody]( localHost, localPort, remoteHost, remotePort )( flip )
    }
  }

/* -------------------------------------------------------------------------- *
 * To get a bilink get the right hand value of the setup return
 * -------------------------------------------------------------------------- */

  def oneBilink[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
    localHost : String = serverHostName, localPort : Int = serverPort,
    remoteHost : String = clientHostName, remotePort : Int = clientPort
  ) : ( EvaluationRequestChannel[ReqBody,RspBody], EvaluationRequestChannel[ReqBody,RspBody] ) = {
    val Right( ( client, server ) ) =
      setup[ReqBody,RspBody]( localHost, localPort, remoteHost, remotePort )( true )
    ( client, server )
  }

  def biLink[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
    localHost : String = serverHostName, localPort : Int = serverPort,
    remoteHostsNPorts : List[(String,Int)] = clientHostsNPorts
  ) : List[( EvaluationRequestChannel[ReqBody,RspBody], EvaluationRequestChannel[ReqBody,RspBody] )] = {
    for( ( remoteHost, remotePort ) <- remoteHostsNPorts )
    yield {
      oneBilink[ReqBody,RspBody]( localHost, localPort, remoteHost, remotePort )
    }
  }

/* -------------------------------------------------------------------------- *
 * To get a single stdlink get the left hand value of the setup return
 * -------------------------------------------------------------------------- */

  def oneStdLink(
    localHost : String = serverHostName, localPort : Int = serverPort,
    remoteHost : String = clientHostName, remotePort : Int = clientPort
  )( flip : Boolean = false ) : StdEvaluationRequestChannel = {
    val Left( client ) =
      setup[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse](
        localHost, localPort, remoteHost, remotePort
      )( false, flip )
    client
  }

  def stdLink(
    localHost : String = serverHostName, localPort : Int = serverPort,
    remoteHostsNPorts : List[(String,Int)] = clientHostsNPorts
  )( flip : Boolean = false ) : List[StdEvaluationRequestChannel] = {
    for( ( remoteHost, remotePort ) <- remoteHostsNPorts )
    yield {
      oneStdLink( localHost, localPort, remoteHost, remotePort )( flip )
    }
  }

/* -------------------------------------------------------------------------- *
 * To get a single stdbilink get the right hand value of the setup return
 * -------------------------------------------------------------------------- */

  def oneStdBiLink(
    localHost : String = serverHostName, localPort : Int = serverPort,
    remoteHost : String = clientHostName, remotePort : Int = clientPort
  ) : ( StdEvaluationRequestChannel, StdEvaluationRequestChannel ) = {
    val Right( ( client, server ) ) =
      setup[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]( localHost, localPort, remoteHost, remotePort )( true )
    ( client, server )
  }

  def stdBiLink(
    localHost : String = serverHostName, localPort : Int = serverPort,
    remoteHostsNPorts : List[(String,Int)] = clientHostsNPorts
  ) : List[( StdEvaluationRequestChannel, StdEvaluationRequestChannel )] = {
    for( ( remoteHost, remotePort ) <- remoteHostsNPorts )
    yield {
      oneStdBiLink( localHost, localPort, remoteHost, remotePort )
    }
  }
}
