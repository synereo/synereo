// -*- mode: Scala;-*- 
// Filename:    BFactory.scala 
// Authors:     lgm                                                    
// Creation:    Sat Apr 27 00:25:52 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

import com.biosimilarity.evaluator.dsl.bfact._

import com.protegra_ati.agentservices.store._

import com.protegra_ati.agentservices.store.extensions.URIExtensions._
//import com.protegra_ati.agentservices.store.extensions.URMExtensions._
import com.protegra_ati.agentservices.store.extensions.MonikerExtensions._

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.store._
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

import com.rabbitmq.client._

import org.prolog4j._

import com.mongodb.casbah.Imports._

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
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

package bfactory {
  import scala.xml._
  import scala.xml.XML._
  import scala.collection.mutable.Buffer
  import scala.collection.mutable.ListBuffer

  object BFactoryMirror {
    @transient
    lazy val ru = scala.reflect.runtime.universe
    def instantiateBehavior(
      behavior : String
    ) : Either[( Any, ru.Mirror, ru.Type ), Throwable] = {
      try {        
        val m = ru.runtimeMirror( getClass.getClassLoader )
        val clsSym = m.staticClass( behavior )
        val cm = m.reflectClass( clsSym )
        val clsTyp = clsSym.toType
        val ctorDecl = clsTyp.declaration( ru.nme.CONSTRUCTOR )
        val ctors = ctorDecl.asTerm.alternatives
        ctors match {
          case ctor :: rCtors => {
            BasicLogService.tweet(
              "method: commenceInstance"
              + "\n instantiating instance "
              + "\nthis: " + this
              + "\n-----------------------------------------"
              + "\nbehavior: " + behavior
              + "\nctor: " + ctor
            )
            try {
              val ctorm = cm.reflectConstructor( ctor.asMethod )
              val instance = ctorm() //( cnxns, filters )
              Left[( Any, ru.Mirror, ru.Type ), Throwable]( ( instance, m, clsTyp ) )
            }
            catch {
              case e : Throwable => {
                BasicLogService.tweet(
                  "method: commenceInstance"
                  + "\n failed instantiating instance when invoking ctor"
                  + "\nthis: " + this
                  + "\n-----------------------------------------"
                  + "\nbehavior: " + behavior
                  + "\nctor: " + ctor
                )
                
                Right[( Any, ru.Mirror, ru.Type ), Throwable]( e )
              }
            }
          }
          case _ => {
            BasicLogService.tweet(
              "method: commenceInstance"
              + "\n failed in attempt to instantiate instance "
              + "\nthis: " + this
              + "\n-----------------------------------------"
              + "\nbehavior: " + behavior
            )
            Right[( Any, ru.Mirror, ru.Type ), Throwable]( new Exception( "no ctor alternatives: " + ctors ) )
          }
        }
      }
      catch {
        case e : Throwable => {
          Right[( Object, ru.Mirror, ru.Type ), Throwable]( e )
        }
      }
    }

    def instanceEntryPoint(
      behavior : String,
      entryPointMethodName : String
    ) : Either[ru.MethodMirror, Throwable] = {
      try {
        instantiateBehavior( behavior ) match {
          case Left( triple ) => {
            val ( instance, m, clsTyp ) = triple
            val instanceM = m.reflect( instance )
            val instanceEntryPointS =
              clsTyp.declaration(
                ru.newTermName( entryPointMethodName )
              ).asMethod
            val instanceEntryPointM =
              instanceM.reflectMethod( instanceEntryPointS )        
            Left[ru.MethodMirror, Throwable]( instanceEntryPointM )
          }
          case Right( e ) => {
            Right[ru.MethodMirror, Throwable]( e )
          }
        }       
      }
      catch {
        case e : Throwable => {
          BasicLogService.tweet(
            "method: commenceInstance"
            + "\n failed instantiating instance "
            + "\nthis: " + this
            + "\n-----------------------------------------"
            + "\nbehavior: " + behavior
          )
          
          Right[ru.MethodMirror, Throwable]( e )
        }
      }
    }
  }

  object BFactoryEngineScope
         extends AgentKVDBMongoNodeScope[String,String,String,ConcreteBFactHL.BFactHLExpr]
         with UUIDOps
         with Serializable
  {
    import SpecialKURIDefaults._
    import identityConversions._

    type ACTypes = AgentCnxnTypes
    object TheACT extends ACTypes
    override def protoAgentCnxnTypes : ACTypes = TheACT

    type MTTypes = MonadicTermTypes[String,String,String,ConcreteBFactHL.BFactHLExpr]
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
      override def protoDrsp : DRsp = MDGetResponse( aLabel, ConcreteBFactHL.Noop )
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

    object Being extends AgentPersistenceScope with Serializable {      
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

      object AgentKVDBNodeFactory
             extends BaseAgentKVDBNodeFactoryT
             with AgentKVDBNodeFactoryT
             with WireTap
             with Serializable {                       
        type AgentCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse] = AgentKVDB[ReqBody,RspBody]
        //type AgentNode[Rq <: PersistedKVDBNodeRequest, Rs <: PersistedKVDBNodeResponse] = AgentKVDBNode[Rq,Rs]

        override def tap [A] ( fact : A ) : Unit = {
          BasicLogService.reportage( fact )
        }

        override def mkCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( 
          here : URI,
          configFileName : Option[String]
        ) : AgentCache[ReqBody,RspBody] = {
          new AgentKVDB[ReqBody, RspBody](
            MURI( here ),
            configFileName
          ) with Blobify with AMQPMonikerOps {          
            override def toXQSafeJSONBlob( x : java.lang.Object ) : String = {
              new XStream( new JettisonMappedXmlDriver() ).toXML( x )
            }
            override def fromXQSafeJSONBlob( blob : String ) : java.lang.Object = {              
              new XStream( new JettisonMappedXmlDriver() ).fromXML( blob )
            }      
            class StringMongoDBManifest(
              override val storeUnitStr : String,
              @transient override val labelToNS : Option[String => String],
              @transient override val textToVar : Option[String => String],
              @transient override val textToTag : Option[String => String],
              @transient override val textToValue: Option[String => ConcreteBFactHL.BFactHLExpr] = throw new Exception("You need to supply this, dummy")
            )
            extends MongoDBManifest( /* database */ ) {
              override def valueStorageType : String = {
                throw new Exception( "valueStorageType not overriden in instantiation" )
              }
              override def continuationStorageType : String = {
                throw new Exception( "continuationStorageType not overriden in instantiation" )
              }
              
              override def storeUnitStr[Src,Label,Trgt]( cnxn : Cnxn[Src,Label,Trgt] ) : String = {     
                cnxn match {
                  case CCnxn( s, l, t ) => s.toString + l.toString + t.toString
                  case acT.AgentCnxn( s, l, t ) => s.getHost + l.toString + t.getHost
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
              
              def asCacheValue(
                ccl : CnxnCtxtLabel[String,String,String]
              ) : ConcreteBFactHL.BFactHLExpr = {
                BasicLogService.tweet(
                  "converting to cache value"
                )
                ccl match {
                  case CnxnCtxtBranch(
                    "string",
                    CnxnCtxtLeaf( Left( rv ) ) :: Nil
                  ) => {
                    val unBlob =
                      fromXQSafeJSONBlob( rv )
                    
                    unBlob match {
                      case rsrc : mTT.Resource => {
                        getGV( rsrc ).getOrElse( ConcreteBFactHL.Noop )
                      }
                    }
                  }
                  case _ => {
                    //asPatternString( ccl )
                    throw new Exception( "unexpected value form: " + ccl )
                  }
                }
              }

              override def asIndirection(key: mTT.GetRequest, value: DBObject): mTT.GetRequest = ???

              override def asResource(
                key : mTT.GetRequest, // must have the pattern to determine bindings
                value : DBObject
              ) : emT.PlaceInstance = {
                val ltns =
                  labelToNS.getOrElse(
                    throw new Exception( "must have labelToNS to convert mongo object" )
                  )
                val ttv =
                  textToVar.getOrElse(
                    throw new Exception( "must have textToVar to convert mongo object" )
                  )
                val ttt =
                  textToTag.getOrElse(
                    throw new Exception( "must have textToTag to convert mongo object" )
                  )
                //val ttt = ( x : String ) => x
                
                //val ptn = asPatternString( key )
                //println( "ptn : " + ptn )             
                
                CnxnMongoObjectifier.fromMongoObject( value )( ltns, ttv, ttt ) match {
                  case CnxnCtxtBranch( ns, CnxnCtxtBranch( kNs, k :: Nil ) :: CnxnCtxtBranch( vNs, v :: Nil ) :: Nil ) => {
                    matchMap( key, k ) match {
                      case Some( soln ) => {
                        if ( compareNameSpace( ns, kvNameSpace ) ) {
                          val cacheValueRslt =
                              asCacheValue( new CnxnCtxtBranch[String,String,String]( "string", v :: Nil ) )
                          BasicLogService.tweet(
                            (
                              " ****************************************** "
                              + "\nBFactory AgentKVDB : "
                              + "\n method : mkCache"
                              + "\n ------------------------------------------ "
		              + "\n computed cacheValue: " + cacheValueRslt
		              + "\n ****************************************** "
                            )
                          )
                          val groundWrapper =
                            mTT.Ground( cacheValueRslt )
                          val boundHMWrapper =
                            mTT.RBoundHM( Some( groundWrapper ), Some( soln ) )
                          val boundWrapper =
                            mTT.asRBoundAList( boundHMWrapper )

                          BasicLogService.tweet(
                            (
                              " ****************************************** "
                              + "\nBFactory AgentKVDB : "
                              + "\n method : mkCache"
		              + "\n ------------------------------------------ "
                              + "\n boundWrapper: " + boundWrapper
		              + " ****************************** "
                            )
                          )

                          val finalRslt =
                            emT.PlaceInstance(
                              k,
                              Left[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]](
                                boundWrapper
                              ),
                              // BUGBUG -- lgm : why can't the compiler determine
                              // that this cast is not necessary?
                              theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
                            )
                          
                          BasicLogService.tweet(
                            (
                              " ****************************************** "
                              + "\nBFactory AgentKVDB : "
                              + "\n method : mkCache"
		              + "\n ------------------------------------------ "
                              + "\n placeInstance: " + finalRslt
		              + " ****************************** "
                            )
                          )
                            
                          finalRslt
                        }
                        else {
                          if ( compareNameSpace( ns, kvKNameSpace ) ) {
                            val mTT.Continuation( ks ) =
                              asCacheK(
                                new CnxnCtxtBranch[String,String,String](
                                  "string",
                                  v :: Nil
                                )
                              )
                            emT.PlaceInstance(
                              k,
                              Right[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( 
                                ks
                              ),
                              // BUGBUG -- lgm : why can't the compiler determine
                              // that this cast is not necessary?
                              theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
                            )
                          }
                          else {
                            throw new Exception( "unexpected namespace : (" + ns + ")" )
                          }
                        }
                      }
                      case None => {
                        //BasicLogService.tweet( "Unexpected matchMap failure: " + key + " " + k )
                        throw new UnificationQueryFilter( key, k, value )
                      }
                    }                                           
                  }
                  case _ => {
                    throw new Exception( "unexpected record format : " + value )
                  }
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
                        //                    "warning: CnxnCtxtLabel method is using XStream"
                        //                  )
                        fromXQSafeJSONBlob( rv )
                      }
                      case "XStream" => {
                        fromXQSafeJSONBlob( rv )
                      }
                      case "Base64" => {
                        val data : Array[Byte] = Base64Coder.decode( rv )
//                         val ois : ObjectInputStream =
//                           new ObjectInputStream( new ByteArrayInputStream(  data ) )
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
              BasicLogService.tweet(
                (
                  "AgentKVDB : "
                  + "\nthis: " + this
                  + "\n method : persistenceManifest "
                )
              )
              val sid = Some( ( s : String ) => recoverFieldName( s ) )
              val deserialize = Some((s: String) => fromXQSafeJSONBlob(s).asInstanceOf[ConcreteBFactHL.BFactHLExpr])
              val kvdb = this;
              Some(
                new StringMongoDBManifest( dfStoreUnitStr, sid, sid, sid, deserialize ) {
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
        override def ptToPt[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
          here : URI, there : URI
        )(
          implicit configFileNameOpt : Option[String] 
        ) : AgentKVDBNode[ReqBody,RspBody] = {
          val node =
            new AgentKVDBNode[ReqBody,RspBody](
              mkCache( MURI( here ), configFileNameOpt ),
              List( MURI( there ) ),
              None,
              configFileNameOpt
            ) {
              override def mkInnerCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( 
                here : URI,
                configFileName : Option[String]
              ) : HashAgentKVDB[ReqBody,RspBody] = {
                BasicLogService.tweet(
                  (
                    "AgentKVDBNode : "
                    + "\nthis: " + this
                    + "\n method : mkInnerCache "
                    + "\n here: " + here
                    + "\n configFileName: " + configFileName
                  )
                )
                new HashAgentKVDB[ReqBody, RspBody](
                  MURI( here ),
                  configFileName
                ) with Blobify with AMQPMonikerOps {            
                  override def toXQSafeJSONBlob( x : java.lang.Object ) : String = {
                    new XStream( new JettisonMappedXmlDriver() ).toXML( x )
                  }
                  override def fromXQSafeJSONBlob( blob : String ) : java.lang.Object = {              
                    new XStream( new JettisonMappedXmlDriver() ).fromXML( blob )
                  }      
                  class StringMongoDBManifest(
                    override val storeUnitStr : String,
                    @transient override val labelToNS : Option[String => String],
                    @transient override val textToVar : Option[String => String],
                    @transient override val textToTag : Option[String => String],
                    @transient override val textToValue: Option[String => ConcreteBFactHL.BFactHLExpr] = throw new Exception("You need to supply this, dummy")
                  )
                  extends MongoDBManifest( /* database */ ) {
                    override def valueStorageType : String = {
                      throw new Exception( "valueStorageType not overriden in instantiation" )
                    }
                    override def continuationStorageType : String = {
                      throw new Exception( "continuationStorageType not overriden in instantiation" )
                    }
                    
                    override def storeUnitStr[Src,Label,Trgt]( cnxn : Cnxn[Src,Label,Trgt] ) : String = {     
                      cnxn match {
                        case CCnxn( s, l, t ) => s.toString + l.toString + t.toString
                        case acT.AgentCnxn( s, l, t ) => s.getHost + l.toString + t.getHost
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
                    
                    def asCacheValue(
                      ccl : CnxnCtxtLabel[String,String,String]
                    ) : ConcreteBFactHL.BFactHLExpr = {
                      BasicLogService.tweet(
                        "converting to cache value"
                      )
                      ccl match {
                        case CnxnCtxtBranch(
                          "string",
                          CnxnCtxtLeaf( Left( rv ) ) :: Nil
                        ) => {
                          val unBlob =
                            fromXQSafeJSONBlob( rv )
                          
                          unBlob match {
                            case rsrc : mTT.Resource => {
                              getGV( rsrc ).getOrElse( ConcreteBFactHL.Noop )
                            }
                          }
                        }
                        case _ => {
                          //asPatternString( ccl )
                          throw new Exception( "unexpected value form: " + ccl )
                        }
                      }
                    }

                    override def asIndirection(key: mTT.GetRequest, value: DBObject): mTT.GetRequest = ???

                    override def asResource(
                      key : mTT.GetRequest, // must have the pattern to determine bindings
                      value : DBObject
                    ) : emT.PlaceInstance = {
                      val ltns =
                        labelToNS.getOrElse(
                          throw new Exception( "must have labelToNS to convert mongo object" )
                        )
                      val ttv =
                        textToVar.getOrElse(
                          throw new Exception( "must have textToVar to convert mongo object" )
                        )
                      val ttt =
                        textToTag.getOrElse(
                          throw new Exception( "must have textToTag to convert mongo object" )
                        )
                      //val ttt = ( x : String ) => x
                      
                      //val ptn = asPatternString( key )
                      //println( "ptn : " + ptn )               
                      
                      CnxnMongoObjectifier.fromMongoObject( value )( ltns, ttv, ttt ) match {
                        case CnxnCtxtBranch( ns, CnxnCtxtBranch( kNs, k :: Nil ) :: CnxnCtxtBranch( vNs, v :: Nil ) :: Nil ) => {
                          matchMap( key, k ) match {
                            case Some( soln ) => {
                              if ( compareNameSpace( ns, kvNameSpace ) ) {
                                val cacheValueRslt =
                                  asCacheValue( new CnxnCtxtBranch[String,String,String]( "string", v :: Nil ) )
                                BasicLogService.tweet(
                                  (
                                    " ****************************************** "
                                    + "\nBFactory AgentKVDB : "
                                    + "\n method : mkCache"
                                    + "\n ------------------------------------------ "
		                    + "\n computed cacheValue: " + cacheValueRslt
		                    + "\n ****************************************** "
                                  )
                                )
                                val groundWrapper =
                                  mTT.Ground( cacheValueRslt )
                                val boundHMWrapper =
                                  mTT.RBoundHM( Some( groundWrapper ), Some( soln ) )
                                val boundWrapper =
                                  mTT.asRBoundAList( boundHMWrapper )
                                
                                BasicLogService.tweet(
                                  (
                                    " ****************************************** "
                                    + "\nBFactory AgentKVDB : "
                                    + "\n method : mkCache"
		                    + "\n ------------------------------------------ "
                                    + "\n boundWrapper: " + boundWrapper
		                    + " ****************************** "
                                  )
                                )
                                
                                val finalRslt =
                                  emT.PlaceInstance(
                                    k,
                                    Left[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]](
                                      boundWrapper
                                    ),
                                    // BUGBUG -- lgm : why can't the compiler determine
                                    // that this cast is not necessary?
                                    theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
                                  )
                                
                                BasicLogService.tweet(
                                  (
                                    " ****************************************** "
                                    + "\nBFactory AgentKVDB : "
                                    + "\n method : mkCache"
		                    + "\n ------------------------------------------ "
                                    + "\n placeInstance: " + finalRslt
		                    + " ****************************** "
                                  )
                                )
                                
                                finalRslt
                              }
                              else {
                                if ( compareNameSpace( ns, kvKNameSpace ) ) {
                                  val mTT.Continuation( ks ) =
                                    asCacheK(
                                      new CnxnCtxtBranch[String,String,String](
                                        "string",
                                        v :: Nil
                                      )
                                    )
                                  emT.PlaceInstance(
                                    k,
                                    Right[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( 
                                      ks
                                    ),
                                    // BUGBUG -- lgm : why can't the compiler determine
                                    // that this cast is not necessary?
                                    theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
                                  )
                                }
                                else {
                                  throw new Exception( "unexpected namespace : (" + ns + ")" )
                                }
                              }
                            }
                            case None => {
                              //BasicLogService.tweet( "Unexpected matchMap failure: " + key + " " + k )
                              throw new UnificationQueryFilter( key, k, value )
                            }
                          }                                             
                        }
                        case _ => {
                          throw new Exception( "unexpected record format : " + value )
                        }
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
//                               val ois : ObjectInputStream =
//                                 new ObjectInputStream( new ByteArrayInputStream(  data ) )
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
                    BasicLogService.tweet(
                      (
                        "HashAgentKVDB : "
                        + "\nthis: " + this
                        + "\n method : persistenceManifest "
                      )
                    )
                    val sid = Some( ( s : String ) => recoverFieldName( s ) )
                    val deserialize = Some((s: String) => fromXQSafeJSONBlob(s).asInstanceOf[ConcreteBFactHL.BFactHLExpr])
                    val kvdb = this;
                    Some(
                      new StringMongoDBManifest( dfStoreUnitStr, sid, sid, sid, deserialize ) {
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
            }
          spawn {
            node.dispatchDMsgs()
          }
          node
        }
        override def ptToMany[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
          here : URI, there : List[URI]
        )(
          implicit configFileNameOpt : Option[String]
        ) : AgentKVDBNode[ReqBody,RspBody] = {
          val node =
            new AgentKVDBNode[ReqBody,RspBody](
              mkCache( MURI( here ), configFileNameOpt ),
              there.map( MURI( _ ) ),
              None,
              configFileNameOpt
            ) {
              override def mkInnerCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( 
                here : URI,
                configFileName : Option[String]
              ) : HashAgentKVDB[ReqBody,RspBody] = {
                BasicLogService.tweet(
                  (
                    "AgentKVDBNode : "
                    + "\nthis: " + this
                    + "\n method : mkInnerCache "
                    + "\n here: " + here
                    + "\n configFileName: " + configFileName
                  )
                )
                new HashAgentKVDB[ReqBody, RspBody](
                  MURI( here ),
                  configFileName
                ) with Blobify with AMQPMonikerOps {            
                  override def toXQSafeJSONBlob( x : java.lang.Object ) : String = {
                    new XStream( new JettisonMappedXmlDriver() ).toXML( x )
                  }
                  override def fromXQSafeJSONBlob( blob : String ) : java.lang.Object = {              
                    new XStream( new JettisonMappedXmlDriver() ).fromXML( blob )
                  }      
                  class StringMongoDBManifest(
                    override val storeUnitStr : String,
                    @transient override val labelToNS : Option[String => String],
                    @transient override val textToVar : Option[String => String],
                    @transient override val textToTag : Option[String => String],
                    @transient override val textToValue: Option[String => ConcreteBFactHL.BFactHLExpr] = throw new Exception("You need to supply this, dummy")
                  )
                  extends MongoDBManifest( /* database */ ) {
                    override def valueStorageType : String = {
                      throw new Exception( "valueStorageType not overriden in instantiation" )
                    }
                    override def continuationStorageType : String = {
                      throw new Exception( "continuationStorageType not overriden in instantiation" )
                    }
                    
                    override def storeUnitStr[Src,Label,Trgt]( cnxn : Cnxn[Src,Label,Trgt] ) : String = {     
                      cnxn match {
                        case CCnxn( s, l, t ) => s.toString + l.toString + t.toString
                        case acT.AgentCnxn( s, l, t ) => s.getHost + l.toString + t.getHost
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
                    
                    def asCacheValue(
                      ccl : CnxnCtxtLabel[String,String,String]
                    ) : ConcreteBFactHL.BFactHLExpr = {
                      BasicLogService.tweet(
                        "converting to cache value"
                      )
                      ccl match {
                        case CnxnCtxtBranch(
                          "string",
                          CnxnCtxtLeaf( Left( rv ) ) :: Nil
                        ) => {
                          val unBlob =
                            fromXQSafeJSONBlob( rv )
                          
                          unBlob match {
                            case rsrc : mTT.Resource => {
                              BasicLogService.tweet(
                                "**********************************************"
	                        +"\n method : asCacheValue"
                                +"\n rsrc is mTT.Resource"
                                +"\n this : " + this
                                +"\n ccl : " + ccl
                                +"\n----------------------------------------------"
                                +"\n unBlob : " + unBlob
                                +"\n**********************************************"
	                      )
                              getGV( rsrc ).getOrElse( ConcreteBFactHL.Noop )
                            }
                            case rsrcz : MonadicTermTypes[_,_,_,_]#Resource => {
                              BasicLogService.tweet(
                                "**********************************************"
	                        +"\n method : asCacheValue"
                                +"\n rsrc is MonadicTermTypes[_,_,_,_]#Resource"
                                +"\n this : " + this
                                +"\n ccl : " + ccl
                                +"\n----------------------------------------------"
                                +"\n unBlob : " + unBlob
                                +"\n**********************************************"
	                      )
                              getGV( rsrcz.asInstanceOf[mTT.Resource] ).getOrElse( ConcreteBFactHL.Noop )
                            }
                            case _ => {
                              if ( unBlob.getClass.getName.equals( "com.biosimilarity.lift.model.store.MonadicTermTypes$Ground" ) ) {
                                BasicLogService.tweet(
                                  "**********************************************"
	                          +"\n method : asCacheValue"
                                  +"\n last ditch effort"
                                  +"\n unBlob.getClass.getName.equals( \"com.biosimilarity.lift.model.store.MonadicTermTypes$Ground\" )"
                                  +"\n this : " + this
                                  +"\n ccl : " + ccl
                                  +"\n----------------------------------------------"
                                  +"\n unBlob : " + unBlob
                                  +"\n**********************************************"
	                        )
                                getGV( unBlob.asInstanceOf[mTT.Resource] ).getOrElse( ConcreteBFactHL.Noop )
                              }
                              else {
                                throw new Exception( "unexpected value form: " + ccl )
                              }
                            }
                          }
                        }
                        case _ => {
                          //asPatternString( ccl )
                          throw new Exception( "unexpected value form: " + ccl )
                        }
                      }
                    }

                    override def asIndirection(key: mTT.GetRequest, value: DBObject): mTT.GetRequest = ???

                    override def asResource(
                      key : mTT.GetRequest, // must have the pattern to determine bindings
                      value : DBObject
                    ) : emT.PlaceInstance = {
                      val ltns =
                        labelToNS.getOrElse(
                          throw new Exception( "must have labelToNS to convert mongo object" )
                        )
                      val ttv =
                        textToVar.getOrElse(
                          throw new Exception( "must have textToVar to convert mongo object" )
                        )
                      val ttt =
                        textToTag.getOrElse(
                          throw new Exception( "must have textToTag to convert mongo object" )
                        )
                      //val ttt = ( x : String ) => x
                      
                      //val ptn = asPatternString( key )
                      //println( "ptn : " + ptn )               
                      
                      CnxnMongoObjectifier.fromMongoObject( value )( ltns, ttv, ttt ) match {
                        case CnxnCtxtBranch( ns, CnxnCtxtBranch( kNs, k :: Nil ) :: CnxnCtxtBranch( vNs, v :: Nil ) :: Nil ) => {
                          matchMap( key, k ) match {
                            case Some( soln ) => {
                              if ( compareNameSpace( ns, kvNameSpace ) ) {
                                val cacheValueRslt =
                                  asCacheValue( new CnxnCtxtBranch[String,String,String]( "string", v :: Nil ) )
                                BasicLogService.tweet(
                                  (
                                    " ****************************************** "
                                    + "\nBFactory AgentKVDB : "
                                    + "\n method : mkCache"
                                    + "\n ------------------------------------------ "
		                    + "\n computed cacheValue: " + cacheValueRslt
		                    + "\n ****************************************** "
                                  )
                                )
                                val groundWrapper =
                                  mTT.Ground( cacheValueRslt )
                                val boundHMWrapper =
                                  mTT.RBoundHM( Some( groundWrapper ), Some( soln ) )
                                val boundWrapper =
                                  mTT.asRBoundAList( boundHMWrapper )
                                
                                BasicLogService.tweet(
                                  (
                                    " ****************************************** "
                                    + "\nBFactory AgentKVDB : "
                                    + "\n method : mkCache"
		                    + "\n ------------------------------------------ "
                                    + "\n boundWrapper: " + boundWrapper
		                    + " ****************************** "
                                  )
                                )
                                
                                val finalRslt =
                                  emT.PlaceInstance(
                                    k,
                                    Left[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]](
                                      boundWrapper
                                    ),
                                    // BUGBUG -- lgm : why can't the compiler determine
                                    // that this cast is not necessary?
                                    theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
                                  )
                                
                                BasicLogService.tweet(
                                  (
                                    " ****************************************** "
                                    + "\nBFactory AgentKVDB : "
                                    + "\n method : mkCache"
		                    + "\n ------------------------------------------ "
                                    + "\n placeInstance: " + finalRslt
		                    + " ****************************** "
                                  )
                                )
                                
                                finalRslt
                              }
                              else {
                                if ( compareNameSpace( ns, kvKNameSpace ) ) {
                                  val mTT.Continuation( ks ) =
                                    asCacheK(
                                      new CnxnCtxtBranch[String,String,String](
                                        "string",
                                        v :: Nil
                                      )
                                    )
                                  emT.PlaceInstance(
                                    k,
                                    Right[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( 
                                      ks
                                    ),
                                    // BUGBUG -- lgm : why can't the compiler determine
                                    // that this cast is not necessary?
                                    theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
                                  )
                                }
                                else {
                                  throw new Exception( "unexpected namespace : (" + ns + ")" )
                                }
                              }
                            }
                            case None => {
                              //BasicLogService.tweet( "Unexpected matchMap failure: " + key + " " + k )
                              throw new UnificationQueryFilter( key, k, value )
                            }
                          }                                             
                        }
                        case _ => {
                          throw new Exception( "unexpected record format : " + value )
                        }
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
//                               val ois : ObjectInputStream =
//                                 new ObjectInputStream( new ByteArrayInputStream(  data ) )
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
                    BasicLogService.tweet(
                      (
                        "HashAgentKVDB : "
                        + "\nthis: " + this
                        + "\n method : persistenceManifest "
                      )
                    )
                    val sid = Some( ( s : String ) => recoverFieldName( s ) )
                    val deserialize = Some((s: String) => fromXQSafeJSONBlob(s).asInstanceOf[ConcreteBFactHL.BFactHLExpr])
                    val kvdb = this;
                    Some(
                      new StringMongoDBManifest( dfStoreUnitStr, sid, sid, sid, deserialize ) {
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
            }
          spawn {
            BasicLogService.tweet( "initiating dispatch on " + node )
            node.dispatchDMsgs()
          }
          node
        }
        def loopBack[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
          here : URI
        )(
          implicit configFileNameOpt : Option[String]
        ) : AgentKVDBNode[ReqBody,RspBody] = {
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
            new AgentKVDBNode[ReqBody, RspBody](
              mkCache( MURI( hereNow ), configFileNameOpt ),
              List( MURI( thereNow ) ),
              None,
              configFileNameOpt
            ) {
              override def mkInnerCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( 
                here : URI,
                configFileName : Option[String]
              ) : HashAgentKVDB[ReqBody,RspBody] = {
                BasicLogService.tweet(
                  (
                    "AgentKVDBNode : "
                    + "\nthis: " + this
                    + "\n method : mkInnerCache "
                    + "\n here: " + here
                    + "\n configFileName: " + configFileName
                  )
                )
                new HashAgentKVDB[ReqBody, RspBody](
                  MURI( here ),
                  configFileName
                ) with Blobify with AMQPMonikerOps {            
                  override def toXQSafeJSONBlob( x : java.lang.Object ) : String = {
                    new XStream( new JettisonMappedXmlDriver() ).toXML( x )
                  }
                  override def fromXQSafeJSONBlob( blob : String ) : java.lang.Object = {              
                    new XStream( new JettisonMappedXmlDriver() ).fromXML( blob )
                  }      
                  class StringMongoDBManifest(
                    override val storeUnitStr : String,
                    @transient override val labelToNS : Option[String => String],
                    @transient override val textToVar : Option[String => String],
                    @transient override val textToTag : Option[String => String],
                    @transient override val textToValue: Option[String => ConcreteBFactHL.BFactHLExpr] = throw new Exception("You need to supply this value dummy")
                  )
                  extends MongoDBManifest( /* database */ ) {
                    override def valueStorageType : String = {
                      throw new Exception( "valueStorageType not overriden in instantiation" )
                    }
                    override def continuationStorageType : String = {
                      throw new Exception( "continuationStorageType not overriden in instantiation" )
                    }
                    
                    override def storeUnitStr[Src,Label,Trgt]( cnxn : Cnxn[Src,Label,Trgt] ) : String = {     
                      cnxn match {
                        case CCnxn( s, l, t ) => s.toString + l.toString + t.toString
                        case acT.AgentCnxn( s, l, t ) => s.getHost + l.toString + t.getHost
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
                    
                    def asCacheValue(
                      ccl : CnxnCtxtLabel[String,String,String]
                    ) : ConcreteBFactHL.BFactHLExpr = {
                      BasicLogService.tweet(
                        "converting to cache value"
                      )
                      ccl match {
                        case CnxnCtxtBranch(
                          "string",
                          CnxnCtxtLeaf( Left( rv ) ) :: Nil
                        ) => {
                          val unBlob =
                            fromXQSafeJSONBlob( rv )
                          
                          unBlob match {
                            case rsrc : mTT.Resource => {
                              getGV( rsrc ).getOrElse( ConcreteBFactHL.Noop )
                            }
                          }
                        }
                        case _ => {
                          //asPatternString( ccl )
                          throw new Exception( "unexpected value form: " + ccl )
                        }
                      }
                    }

                    override def asIndirection(key: mTT.GetRequest, value: DBObject): mTT.GetRequest = ???

                    override def asResource(
                      key : mTT.GetRequest, // must have the pattern to determine bindings
                      value : DBObject
                    ) : emT.PlaceInstance = {
                      val ltns =
                        labelToNS.getOrElse(
                          throw new Exception( "must have labelToNS to convert mongo object" )
                        )
                      val ttv =
                        textToVar.getOrElse(
                          throw new Exception( "must have textToVar to convert mongo object" )
                        )
                      val ttt =
                        textToTag.getOrElse(
                          throw new Exception( "must have textToTag to convert mongo object" )
                        )
                      //val ttt = ( x : String ) => x
                      
                      //val ptn = asPatternString( key )
                      //println( "ptn : " + ptn )               
                      
                      CnxnMongoObjectifier.fromMongoObject( value )( ltns, ttv, ttt ) match {
                        case CnxnCtxtBranch( ns, CnxnCtxtBranch( kNs, k :: Nil ) :: CnxnCtxtBranch( vNs, v :: Nil ) :: Nil ) => {
                          matchMap( key, k ) match {
                            case Some( soln ) => {
                              if ( compareNameSpace( ns, kvNameSpace ) ) {
                                val cacheValueRslt =
                                  asCacheValue( new CnxnCtxtBranch[String,String,String]( "string", v :: Nil ) )
                                BasicLogService.tweet(
                                  (
                                    " ****************************************** "
                                    + "\nBFactory AgentKVDB : "
                                    + "\n method : mkCache"
                                    + "\n ------------------------------------------ "
		                    + "\n computed cacheValue: " + cacheValueRslt
		                    + "\n ****************************************** "
                                  )
                                )
                                val groundWrapper =
                                  mTT.Ground( cacheValueRslt )
                                val boundHMWrapper =
                                  mTT.RBoundHM( Some( groundWrapper ), Some( soln ) )
                                val boundWrapper =
                                  mTT.asRBoundAList( boundHMWrapper )
                                
                                BasicLogService.tweet(
                                  (
                                    " ****************************************** "
                                    + "\nBFactory AgentKVDB : "
                                    + "\n method : mkCache"
		                    + "\n ------------------------------------------ "
                                    + "\n boundWrapper: " + boundWrapper
		                    + " ****************************** "
                                  )
                                )
                                
                                val finalRslt =
                                  emT.PlaceInstance(
                                    k,
                                    Left[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]](
                                      boundWrapper
                                    ),
                                    // BUGBUG -- lgm : why can't the compiler determine
                                    // that this cast is not necessary?
                                    theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
                                  )
                                
                                BasicLogService.tweet(
                                  (
                                    " ****************************************** "
                                    + "\nBFactory AgentKVDB : "
                                    + "\n method : mkCache"
		                    + "\n ------------------------------------------ "
                                    + "\n placeInstance: " + finalRslt
		                    + " ****************************** "
                                  )
                                )
                                
                                finalRslt
                              }
                              else {
                                if ( compareNameSpace( ns, kvKNameSpace ) ) {
                                  val mTT.Continuation( ks ) =
                                    asCacheK(
                                      new CnxnCtxtBranch[String,String,String](
                                        "string",
                                        v :: Nil
                                      )
                                    )
                                  emT.PlaceInstance(
                                    k,
                                    Right[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( 
                                      ks
                                    ),
                                    // BUGBUG -- lgm : why can't the compiler determine
                                    // that this cast is not necessary?
                                    theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
                                  )
                                }
                                else {
                                  throw new Exception( "unexpected namespace : (" + ns + ")" )
                                }
                              }
                            }
                            case None => {
                              //BasicLogService.tweet( "Unexpected matchMap failure: " + key + " " + k )
                              throw new UnificationQueryFilter( key, k, value )
                            }
                          }                                             
                        }
                        case _ => {
                          throw new Exception( "unexpected record format : " + value )
                        }
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
//                               val ois : ObjectInputStream =
//                                 new ObjectInputStream( new ByteArrayInputStream(  data ) )
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
                    BasicLogService.tweet(
                      (
                        "HashAgentKVDB : "
                        + "\nthis: " + this
                        + "\n method : persistenceManifest "
                      )
                    )
                    val sid = Some( ( s : String ) => recoverFieldName( s ) )
                    val deserialize = Some((s: String) => fromXQSafeJSONBlob(s).asInstanceOf[ConcreteBFactHL.BFactHLExpr])
                    val kvdb = this;
                    Some(
                      new StringMongoDBManifest( dfStoreUnitStr, sid, sid, sid, deserialize ) {
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
            }
          spawn {
            BasicLogService.tweet( "initiating dispatch on " + node )
            node.dispatchDMsgs()
          }
          node
        }
      }
    }

  }

  trait BFactoryEvaluatorConfiguration {
    self : EvalConfig =>
      def dslEvaluatorHostName() : String = {
        try {
          evalConfig().getString( "BFactoryEvaluatorHost" )
        }
        catch {
          case e : Throwable => "localhost" 
        }
      }
    def dslEvaluatorHostPort() : Int = {
      try {
        evalConfig().getInt( "BFactoryEvaluatorPort" )
      }
      catch {
        case e : Throwable => 5672
      }
    }
    def dslEvaluatorHostData() : String = {
        try {
          evalConfig().getString( "BFactoryEvaluatorHostData" )
        }
        catch {
          case e : Throwable => "/bFactoryProtocol" 
        }
      }
    def dslEvaluatorPreferredSupplierHostName() : String = {
        try {
          evalConfig().getString( "BFactoryEvaluatorPreferredSupplierHost" )
        }
        catch {
          case e : Throwable => "localhost" 
        }
      }
    def dslEvaluatorPreferredSupplierPort() : Int = {
      try {
        evalConfig().getInt( "BFactoryEvaluatorPreferredSupplierPort" )
      }
      catch {
        case e : Throwable => 5672
      }
    }
    def dslEvaluatorNetwork() : List[String] = {
      import scala.collection.JavaConverters._
      try {
        val rslt : java.util.List[String] =
          evalConfig().getStringList( "BFactoryEvaluatorNetwork" )
        rslt.asScala.toList
      }
      catch {
        case e : Throwable => List[String]( )
      }
    }
  }

  object BFactoryConfigurationDefaults extends Serializable {
    val localHost : String = "localhost"
    val localPort : Int = 5672
    val remoteHost : String = "localhost"
    val remotePort : Int = 5672
    val dataLocation : String = "/cnxnTestProtocol"    
  }

  trait BFactoryManufactureConfiguration extends ConfigurationTrampoline {
    def localHost : String =
      configurationFromFile.get( "localHost" ).getOrElse( bail() )
    def localPort : Int =
      configurationFromFile.get( "localPort" ).getOrElse( bail() ).toInt
    def remoteHost : String =
      configurationFromFile.get( "remoteHost" ).getOrElse( bail() )
    def remotePort : Int =
      configurationFromFile.get( "remotePort" ).getOrElse( bail() ).toInt    
    def dataLocation : String = 
      configurationFromFile.get( "dataLocation" ).getOrElse( bail() )
  }

  object BFactoryEngineCtor extends EvalConfig
  with BFactoryCommLinkConfiguration
  with BFactoryEvaluatorConfiguration
  with Serializable {
    import BFactoryEngineScope._
    import Being._
    import AgentKVDBNodeFactory._

    import CnxnConversionStringScope._

    import com.protegra_ati.agentservices.store.extensions.StringExtensions._

    type LinkEvalRequestChannel = BFactoryCommLinkCtor.StdEvaluationRequestChannel
    type EvalChannel[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse] = Being.AgentKVDBNode[ReqBody,RspBody]
    type StdEvalChannel = EvalChannel[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]

    type ChannelBundle = ( LinkEvalRequestChannel, LinkEvalRequestChannel, StdEvalChannel )

    def setup[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
      dataLocation : String = dslEvaluatorHostData,
      localHost : String = dslEvaluatorHostName,
      localPort : Int = dslEvaluatorHostPort,
      remoteHost : String = dslEvaluatorPreferredSupplierHostName,
      remotePort : Int = dslEvaluatorPreferredSupplierPort
    )(
      implicit returnTwist : Boolean
    ) : Either[EvalChannel[ReqBody,RspBody],(EvalChannel[ReqBody, RspBody],EvalChannel[ReqBody, RspBody])] = {
      val ( localExchange, remoteExchange ) = 
        if ( localHost.equals( remoteHost ) && ( localPort == remotePort ) ) {
          ( dataLocation, dataLocation + "Remote" )       
        }
        else {
          ( dataLocation, dataLocation )          
        }

      if ( returnTwist ) {
        Right[EvalChannel[ReqBody,RspBody],(EvalChannel[ReqBody, RspBody],EvalChannel[ReqBody, RspBody])](
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
        Left[EvalChannel[ReqBody, RspBody],(EvalChannel[ReqBody, RspBody],EvalChannel[ReqBody, RspBody])](
          ptToPt(
            new URI( "agent", null, localHost, localPort, localExchange, null, null ),
            new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
          )
        )
      }
    }    

    def setup[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
      localHost : String, localPort : Int,
      remoteHost : String, remotePort : Int
    )(
      implicit returnTwist : Boolean
    ) : Either[EvalChannel[ReqBody,RspBody],(EvalChannel[ReqBody, RspBody],EvalChannel[ReqBody, RspBody])] = {
      setup( "/bFactoryProtocol", localHost, localPort, remoteHost, remotePort )
    }

    def setupBFactoryEvaluatorNode[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
      dataLocation : String = dslEvaluatorHostData,
      localHost : String = dslEvaluatorHostName,
      localPort : Int = dslEvaluatorHostPort
    ) : EvalChannel[ReqBody, RspBody] = {      
      ptToMany(
        new URI( "agent", null, localHost, localPort, dataLocation, null, null ),
        List[URI]( )
      )
    }

    def agent[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( 
      dataLocation : String
    ) : EvalChannel[ReqBody,RspBody] = {
      val Right( ( client, server ) ) = 
        setup[ReqBody,RspBody](
          dataLocation, "localhost", 5672, "localhost", 5672
        )( true )
      client
    }    

    def dslEvaluatorAgent[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
    ): EvalChannel[ReqBody,RspBody] = {
      setupBFactoryEvaluatorNode[ReqBody,RspBody]()
    }

    object StorageLabels extends CnxnString[String,String,String] with Serializable {
      def instanceStorageLabel(
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
          "instanceLabel( majorVersion( \""
          + majorVersion
          + "\" ), minorVersion( \""
          + minorVersion
          + "\"), sessionId( "
          + sessionTerm
          + " ) )"
        ).getOrElse( throw new Exception( "unable to build term from string" ) )
      }      
    }

    class BFactoryEngine(
      override val configFileName : Option[String],
      val cnxnGlobal : acT.AgentCnxn = new acT.AgentCnxn("Global".toURI, "", "Global".toURI),
      val version : String = "0.0.1"
    ) extends BFactoryManufactureConfiguration with Serializable {        
      override def configurationDefaults : ConfigurationDefaults = {
        BFactoryConfigurationDefaults.asInstanceOf[ConfigurationDefaults]
      }      
      
      def fileNameToCnxn( fileName : String ) : acT.AgentCnxn = {
        val fileNameRoot = fileName.split( '/' ).last
        new acT.AgentCnxn( fileNameRoot.toURI, "", fileNameRoot.toURI )
      } 

      // BUGBUG : lgm -- this code is not in sync with the weak map
      // case -- please introduce abstraction!!!

      def evaluateExpression[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
        node : EvalChannel[ReqBody,RspBody],
        dslNode : com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.StdEvalChannel
      )( expr : ConcreteBFactHL.BFactHLExpr )(
        handler : Option[mTT.Resource] => Unit
      ): Unit = {
        BasicLogService.tweet(
          "entering method: evaluateExpression"
          + "\nthis: " + this
          + "\nnode: " + node
          + "\nexpr: " + expr
          + "\nhandler: " + handler
        )
        expr match {
          case ConcreteBFactHL.Noop => {
            //throw new Exception( "divergence" )
            //println( "warning: divergent expression" )
            BasicLogService.tweet( "handling noop" )
            handler( None )
          }
          case ConcreteBFactHL.MapBehavior( cnxn, label, behavior ) => {
              BasicLogService.tweet(
                "method: evaluateExpression"
                + "\nin ConcreteBFactHL.MapBehavior case "
                + "\nthis: " + this
                + "\nnode: " + node
                + "\nexpr: " + expr
                + "\nhandler: " + handler
                + "\n-----------------------------------------"
                + "\ncnxn: " + cnxn
                + "\nlabel: " + label
                + "\nbehavior: " + behavior
              )
              
              val agntCnxn : acT.AgentCnxn =
                new acT.AgentCnxn( cnxn.src, cnxn.label.toString, cnxn.trgt )
              reset {
                  
                BasicLogService.tweet(
                  "method: evaluateExpression"
                  + "\n calling node.publish "
                  + "\nthis: " + this
                  + "\nnode: " + node
                  + "\nexpr: " + expr
                  + "\nhandler: " + handler
                  + "\n-----------------------------------------"
                  + "\nagntCnxn: " + agntCnxn
                  + "\nlabel: " + label
                  + "\nbehavior: " + behavior
                )

                try {
                  node.publish( agntCnxn )( label, mTT.Ground( ConcreteBFactHL.WrappedBehaviorIdentifier( behavior ) ) )
                  handler( 
                    Some(
                      mTT.Ground(
                        ConcreteBFactHL.Noop
                      )
                    )
                  )
                } 
                catch {
                  case e : Exception => {
                    BasicLogService.tweet(
                      "method: evaluateExpression"
                      + "\n ---> node.publish caused an exception <--- "
                      + "\nthis: " + this
                      + "\nnode: " + node
                      + "\nexpr: " + expr
                      + "\nhandler: " + handler
                      + "\n-----------------------------------------"
                      + "\nagntCnxn: " + agntCnxn
                      + "\nlabel: " + label
                      + "\nbehavior: " + behavior
                    )
                    BasicLogService.tweetTrace( e )
                  }
                }
                // Need to send back a noop here                
              }
          }
        }
      }

      def evaluateExpression[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
        node : String,
        dslNode : String
      )( expr : ConcreteBFactHL.BFactHLExpr )(
        handler : Option[mTT.Resource] => Unit
      ): Unit = {
        def commenceInstanceHandler(
          n : StdEvalChannel,
          dslN : com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.StdEvalChannel
        )(
          bdc : ConcreteBFactHL.Cnxn,
          bdl : ConcreteBFactHL.Label,
          cnxns : Seq[ConcreteBFactHL.Cnxn],
          filters : Seq[ConcreteBFactHL.Label]
        ) = {
          BasicLogService.tweet(
            "method: evaluateExpression"
            + "\nin ConcreteBFactHL.CommenceInstance case "
            + "\nthis: " + this
            + "\nnode: " + node
            + "\nexpr: " + expr
            + "\nhandler: " + handler
            + "\n-----------------------------------------"
            + "\nbehaviorDefinitionCnxn: " + bdc
            + "\nbehaviorDefinitionLabel: " + bdl
            + "\ncnxns: " + cnxns
            + "\nfilters: " + filters                
          )
          
          val agntCnxn : acT.AgentCnxn =
            new acT.AgentCnxn( bdc.src, bdc.label.toString, bdc.trgt )
          reset {
            
            BasicLogService.tweet(
              "method: evaluateExpression"
              + "\n calling node.fetch "
              + "\nthis: " + this
              + "\nnode: " + node
              + "\nexpr: " + expr
              + "\nhandler: " + handler
              + "\n-----------------------------------------"
              + "\nbehaviorDefinitionCnxn: " + agntCnxn
              + "\nbehaviorDefinitionLabel: " + bdl
            )
            
            for( e <- n.fetch( agntCnxn )( bdl ) ) {
              
              BasicLogService.tweet(
                "method: evaluateExpression"
                + "\n returned from node.fetch "
                + "\nthis: " + this
                + "\nnode: " + node
                + "\nexpr: " + expr
                + "\nhandler: " + handler
                + "\n-----------------------------------------"
                + "\nagntCnxn: " + agntCnxn
                + "\nbehaviorDefinitionLabel: " + bdl
                + "\ne: " + e
              )
              
              e match {
                case Some( mTT.Ground( ConcreteBFactHL.WrappedBehaviorIdentifier( behavior ) ) ) => {    
                  try {
                    BasicLogService.tweet(
                      "method: evaluateExpression | Ground"
                      + "\n instantiating instance & finding entry point"
                      + "\nthis: " + this
                      + "\nnode: " + node
                      + "\nexpr: " + expr
                      + "\nhandler: " + handler
                      + "\n-----------------------------------------"
                      + "\nbehaviorDefinitionCnxn: " + agntCnxn
                      + "\nbehaviorDefinitionLabel: " + bdl
                      + "\nbehavior: " + behavior
                      + "\ncnxns: " + cnxns
                      + "\nfilters: " + filters
                    )
                    println(
                      "method: evaluateExpression | Ground"
                      + "\n instantiating instance & finding entry point"
                      + "\nthis: " + this
                      + "\nnode: " + node
                      + "\nexpr: " + expr
                      + "\nhandler: " + handler
                      + "\n-----------------------------------------"
                      + "\nbehaviorDefinitionCnxn: " + agntCnxn
                      + "\nbehaviorDefinitionLabel: " + bdl
                      + "\nbehavior: " + behavior
                      + "\ncnxns: " + cnxns
                      + "\nfilters: " + filters
                    )
                    BFactoryMirror.instanceEntryPoint( behavior, "run" ) match {
                      case Left( entryPointM ) => {
                        spawn {
                          val instanceID = UUID.randomUUID
                          val instanceLabel =
                            StorageLabels.instanceStorageLabel()( Left[String,String]( instanceID.toString ) )
                          entryPointM( dslN, cnxns, filters )
                          handler( 
                            Some(
                              mTT.Ground(
                                ConcreteBFactHL.InstanceRunning(
                                  bdc,
                                  instanceLabel
                                )
                              )
                            )
                          )
                        }
                      }
                      case Right( e ) => {
                        handler( 
                          Some(
                            mTT.Ground(
                              ConcreteBFactHL.InstanceNotRunning(
                                bdc,
                                bdl,
                                "instantiation failed" + e
                              )
                            )
                          )
                        )
                      }
                    }
                  }
                  catch {
                    case e : Throwable => {
                      handler( 
                        Some(
                          mTT.Ground(
                            ConcreteBFactHL.InstanceNotRunning(
                              bdc,
                              bdl,
                              "instantiation failed" + e
                            )
                          )
                        )
                      )
                    }
                  }                      
                };
                case Some( mTT.RBoundAList( Some( mTT.Ground( ConcreteBFactHL.WrappedBehaviorIdentifier( behavior ))), _ ) ) => {           
                  try {
                    BasicLogService.tweet(
                      "method: evaluateExpression | RBoundAList"
                      + "\n instantiating instance & finding entry point"
                      + "\nthis: " + this
                      + "\nnode: " + node
                      + "\nexpr: " + expr
                      + "\nhandler: " + handler
                      + "\n-----------------------------------------"
                      + "\nbehaviorDefinitionCnxn: " + agntCnxn
                      + "\nbehaviorDefinitionLabel: " + bdl
                      + "\nbehavior: " + behavior
                      + "\ncnxns: " + cnxns
                      + "\nfilters: " + filters
                    )
                    println(
                      "method: evaluateExpression | RBoundAList"
                      + "\n instantiating instance & finding entry point"
                      + "\nthis: " + this
                      + "\nnode: " + node
                      + "\nexpr: " + expr
                      + "\nhandler: " + handler
                      + "\n-----------------------------------------"
                      + "\nbehaviorDefinitionCnxn: " + agntCnxn
                      + "\nbehaviorDefinitionLabel: " + bdl
                      + "\nbehavior: " + behavior
                      + "\ncnxns: " + cnxns
                      + "\nfilters: " + filters
                    )
                    BFactoryMirror.instanceEntryPoint( behavior, "run" ) match {
                      case Left( entryPointM ) => {
                        spawn {
                            val instanceID = UUID.randomUUID
                            val instanceLabel =
                              StorageLabels.instanceStorageLabel()( Left[String,String]( instanceID.toString ) )
                            entryPointM( dslN, cnxns, filters )
                            handler( 
                              Some(
                                mTT.Ground(
                                  ConcreteBFactHL.InstanceRunning(
                                    bdc,
                                    instanceLabel
                                  )
                                )
                              )
                            )
                        }
                      }
                      case Right( e ) => {
                        handler( 
                          Some(
                            mTT.Ground(
                              ConcreteBFactHL.InstanceNotRunning(
                                bdc,
                                bdl,
                                "instantiation failed" + e
                              )
                            )
                          )
                        )
                      }
                    }
                  }
                  catch {
                    case e : Throwable => {
                      handler( 
                        Some(
                          mTT.Ground(
                            ConcreteBFactHL.InstanceNotRunning(
                              bdc,
                              bdl,
                              "instantiation failed" + e
                            )
                          )
                        )
                      )
                    }
                  }                      
                };
                case _ => {
                  handler( 
                    Some(
                      mTT.Ground(
                        ConcreteBFactHL.InstanceNotRunning(
                          bdc,
                          bdl,
                          "unexpected behavior record format: " + e
                        )
                      )
                    )
                  )
                }
              }                                    
            }
          }
        }
        BasicLogService.tweet(
          "entering method: evaluateExpression"
          + "\nthis: " + this
          + "\nnode: " + node
          + "\nexpr: " + expr
          + "\nhandler: " + handler
          + "\n-----------------------------------------"
          + "\n n: " + EvalNodeMapper.get( node )
        )
        for (
          n <- EvalNodeMapper.get( node );
          dslN <- com.biosimilarity.evaluator.distribution.diesel.EvalNodeMapper.get( dslNode )
        ) {
          expr match {
            case ConcreteBFactHL.Noop => {
              //throw new Exception( "divergence" )
              //println( "warning: divergent expression" )
              BasicLogService.tweet( "handling noop" )
              handler( None )
            }
            case ConcreteBFactHL.MapBehavior( cnxn, label, behavior ) => {
              BasicLogService.tweet(
                "method: evaluateExpression"
                + "\nin ConcreteBFactHL.MapBehavior case "
                + "\nthis: " + this
                + "\nnode: " + node
                + "\nexpr: " + expr
                + "\nhandler: " + handler
                + "\n-----------------------------------------"
                + "\ncnxn: " + cnxn
                + "\nlabel: " + label
                + "\nbehavior: " + behavior
              )
              
              val agntCnxn : acT.AgentCnxn =
                new acT.AgentCnxn( cnxn.src, cnxn.label.toString, cnxn.trgt )
              reset {
                  
                BasicLogService.tweet(
                  "method: evaluateExpression"
                  + "\n calling node.publish "
                  + "\nthis: " + this
                  + "\nnode: " + node
                  + "\nexpr: " + expr
                  + "\nhandler: " + handler
                  + "\n-----------------------------------------"
                  + "\nagntCnxn: " + agntCnxn
                  + "\nlabel: " + label
                  + "\nbehavior: " + behavior
                )

                try {
                  n.publish( agntCnxn )( label, mTT.Ground( ConcreteBFactHL.WrappedBehaviorIdentifier( behavior ) ) )
                  handler( 
                    Some(
                      mTT.Ground(
                        ConcreteBFactHL.Noop
                      )
                    )
                  )
                } 
                catch {
                  case e : Exception => {
                    BasicLogService.tweet(
                      "method: evaluateExpression"
                      + "\n ---> node.publish caused an exception <--- "
                      + "\nthis: " + this
                      + "\nnode: " + node
                      + "\nexpr: " + expr
                      + "\nhandler: " + handler
                      + "\n-----------------------------------------"
                      + "\nagntCnxn: " + agntCnxn
                      + "\nlabel: " + label
                      + "\nbehavior: " + behavior
                    )
                    BasicLogService.tweetTrace( e )
                  }
                }
              }
            }
            case ConcreteBFactHL.CommenceInstance( bdc, bdl, cnxns, filters ) => {
              commenceInstanceHandler( n, dslN )( bdc, bdl, cnxns, filters )
            }
            case ConcreteBFactHL.CommenceInstances( bdc, bdls, cnxnsList, filtersList ) => {
              val bdlsNCnxnsNFilters = bdls.zip( cnxnsList ).zip( filtersList )
              for(
                ( ( bdl, cnxns ), filters ) <- bdlsNCnxnsNFilters
              ) {
                commenceInstanceHandler( n, dslN )( bdc, bdl, cnxns, filters )
              }
            }
          }
        }
      }
      
      trait MessageProcessorElements {
        self : Serializable =>
        def erql() : CnxnCtxtLabel[String,String,String]
        def rspLabelCtor() : String => CnxnCtxtLabel[String,String,String]
        def useBiLink() : Option[Boolean]
        def flip() : Boolean
      }

      trait MessageProcessor {
        self : MessageProcessorElements with Serializable =>

        def innerLoop(
          erql : CnxnCtxtLabel[String,String,String],
          client : LinkEvalRequestChannel,
          server : LinkEvalRequestChannel,
          node : StdEvalChannel,
          dslNode : com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.StdEvalChannel,
          rspLabelCtor : String => CnxnCtxtLabel[String,String,String]
        ) : Unit = {
          BasicLogService.tweet(
            "entering method: innerLoop"
            + "\nthis: " + this
            + "\nerql: " + erql
            + "\nclient: " + client
            + "\nserver: " + server
            + "\nnode: " + node            
            + "\nrspLabelCtor: " + rspLabelCtor
          )
            reset { 
              for( e <- client.subscribe( erql ) ) {
                BasicLogService.tweet(
                  "method: innerLoop"
                  + "\n completed client.subscribe "
                  + "\nthis: " + this
                  + "\nerql: " + erql
                  + "\nclient: " + client
                  + "\nserver: " + server
                  + "\nnode: " + node
                  + "\n-----------------------------------------"
                  + "\ne: " + e
                )                
                e match {
                  case Some( boundRsrc@BFactoryCommLink.mTT.RBoundAList( Some( BFactoryCommLink.mTT.Ground( expr ) ), subst ) ) => {
                    BasicLogService.tweet(
                      "method: innerLoop"
                      + "\n case rsrc type: BFactoryCommLink.mTT.RBoundAList"
                      + "\n completed client.subscribe "
                      + "\nthis: " + this
                      + "\nerql: " + erql
                      + "\nclient: " + client
                      + "\nserver: " + server
                      + "\nnode: " + node
                      + "\n-----------------------------------------"
                      + "\ne: " + e
                    )
                    
                    for( map <- boundRsrc.sbst; CnxnCtxtLeaf( Left( sessionId ) ) <- map.get( "SessionId" ) ) {
                      val erspl : CnxnCtxtLabel[String,String,String] = rspLabelCtor( sessionId )
                      
                      val forward : Option[mTT.Resource] => Unit =
                        {
                          ( optRsrc : Option[mTT.Resource] ) => {
                            BasicLogService.tweet(
                              ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                              + "\nBFactory.scala:2065 forward(" + optRsrc + ")"
                              + "\n------------------------------------------------------------------"
                              + "\n defined in method innerLoop"
                              + "\n passed to and called in evaluateExpression"
                              + "\nerql: " + erql
                              + "\nserver: " + server                              
                              + "\n------------------------------------------------------------------"
                              + "\n erspl: " + erspl
                              + "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                            )
                            reset {
                              server.publish(
                                erspl,
                                BFactoryCommLink.mTT.Ground(
                                  optRsrc match {
                                    case None => {                                
                                      ConcreteBFactHL.Noop
                                    }
                                    case Some( mTT.Ground( v ) ) => {
                                      v
                                    }
                                    case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
                                      v
                                    }
                                  }
                                )
                              )       
                            }
                          }
                        }
                      
                      evaluateExpression( node, dslNode )( expr )( forward )
                    }             
                  }
                  case Some( boundRsrc@BFactoryCommLink.mTT.RBoundHM( Some( BFactoryCommLink.mTT.Ground( expr ) ), subst ) ) => {
                    BasicLogService.tweet(
                      "method: innerLoop"
                      + "\n case rsrc type: BFactoryCommLink.mTT.RBoundHM"
                      + "\n completed client.subscribe "
                      + "\nthis: " + this
                      + "\nerql: " + erql
                      + "\nclient: " + client
                      + "\nserver: " + server
                      + "\nnode: " + node
                      + "\n-----------------------------------------"
                      + "\ne: " + e
                    )
                    
                    for( map <- boundRsrc.sbst; CnxnCtxtLeaf( Left( sessionId ) ) <- map.get( "SessionId" ) ) {
                      val erspl : CnxnCtxtLabel[String,String,String] = rspLabelCtor( sessionId )
                      
                      val forward : Option[mTT.Resource] => Unit =
                        {
                          ( optRsrc : Option[mTT.Resource] ) => {                            
                            BasicLogService.tweet(
                              ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                              + "\nBFactory.scala:2106 forward(" + optRsrc + ")"
                              + "\n------------------------------------------------------------------"
                              + "\n defined in method innerLoop"
                              + "\n passed to and called in evaluateExpression"
                              + "\nerql: " + erql
                              + "\nserver: " + server                              
                              + "\n------------------------------------------------------------------"
                              + "\n erspl: " + erspl
                              + "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                            )
                            reset {
                              server.publish(
                                erspl,
                                BFactoryCommLink.mTT.Ground(
                                  optRsrc match {
                                    case None => {                                
                                      ConcreteBFactHL.Noop
                                    }
                                    case Some( mTT.Ground( v ) ) => {
                                      v
                                    }
                                    case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
                                      v
                                    }
                                  }
                                )
                              )       
                            }
                          }
                        }
                      
                      evaluateExpression( node, dslNode )( expr )( forward )
                    }             
                  }
                  case Some( rsrc ) => {
                    rsrc match {
                      case boundRsrc@BFactoryCommLink.mTT.RBoundHM( innerOptRsrc, subst ) => {
                        BasicLogService.tweet(
                          "method: innerLoop"
                          + "\n case rsrc type: BFactoryCommLink.mTT.RBoundHM"
                          + "\n completed client.subscribe "
                          + "\nthis: " + this
                          + "\nerql: " + erql
                          + "\nclient: " + client
                          + "\nserver: " + server
                          + "\nnode: " + node
                          + "\n-----------------------------------------"
                          + "\ne: " + e
                        )
                        
                        innerOptRsrc match {
                          case Some( BFactoryCommLink.mTT.Ground( expr ) ) => {
                            for( map <- boundRsrc.sbst; CnxnCtxtLeaf( Left( sessionId ) ) <- map.get( "SessionId" ) ) {
                              val erspl : CnxnCtxtLabel[String,String,String] = rspLabelCtor( sessionId )
                              
                              val forward : Option[mTT.Resource] => Unit =
                                {
                                  ( optRsrc : Option[mTT.Resource] ) => {
                                    BasicLogService.tweet(
                                      ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                                      + "\nBFactory.scala:2151 forward(" + optRsrc + ")"
                                      + "\n------------------------------------------------------------------"
                                      + "\n defined in method innerLoop"
                                      + "\n passed to and called in evaluateExpression"
                                      + "\nerql: " + erql
                                      + "\nserver: " + server                              
                                      + "\n------------------------------------------------------------------"
                                      + "\n erspl: " + erspl
                                      + "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                                    )
                                    reset {
                                      server.publish(
                                        erspl,
                                        BFactoryCommLink.mTT.Ground(
                                          optRsrc match {
                                            case None => {                                
                                              ConcreteBFactHL.Noop
                                            }
                                            case Some( mTT.Ground( v ) ) => {
                                              v
                                            }
                                            case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
                                              v
                                            }
                                          }
                                        )
                                      )       
                                    }
                                  }
                                }                        
                              evaluateExpression( node, dslNode )( expr )( forward )
                            }
                          }
                          case Some( innerRrsc ) => {
                            BasicLogService.tweet(
                              "method: innerLoop"
                              + "\n case unexpected inner rsrc type: " + innerRrsc
                              + "\ninner rsrc type: " + innerRrsc.getClass
                              + "\n completed client.subscribe "
                              + "\nthis: " + this
                              + "\nerql: " + erql
                              + "\nclient: " + client
                              + "\nserver: " + server
                              + "\nnode: " + node
                              + "\n-----------------------------------------"
                              + "\ne: " + e
                            )
                          }
                        }                      
                      }
                      case _ => {
                        BasicLogService.tweet(
                          "method: innerLoop"
                          + "\n case unexpected rsrc type: " + rsrc
                          + "\ninner rsrc type: " + rsrc.getClass
                          + "\n completed client.subscribe "
                          + "\nthis: " + this
                          + "\nerql: " + erql
                          + "\nclient: " + client
                          + "\nserver: " + server
                          + "\nnode: " + node
                          + "\n-----------------------------------------"
                          + "\ne: " + e
                        )
                      }
                    }             
                  }
                  case None => {
                    BasicLogService.tweet( "server loop waiting." )
                  }
                  case _ => {
                    BasicLogService.tweet(
                      "method: innerLoop"
                      + "\n rsrc not handled: " + e
                      + "\n completed client.subscribe "
                      + "\nthis: " + this
                      + "\nerql: " + erql
                      + "\nclient: " + client
                      + "\nserver: " + server
                      + "\nnode: " + node
                    )
                  }
                }
              }
            }
        }

        def innerLoop(
          erql : CnxnCtxtLabel[String,String,String],
          client : LinkEvalRequestChannel,
          server : LinkEvalRequestChannel,
          node : String,
          dslNode : String,
          rspLabelCtor : String => CnxnCtxtLabel[String,String,String]
        ) : Unit = {
          BasicLogService.tweet(
            "entering method: innerLoop"
            + "\nthis: " + this
            + "\nerql: " + erql
            + "\nclient: " + client
            + "\nserver: " + server
            + "\nnode: " + node            
            + "\nrspLabelCtor: " + rspLabelCtor
          )
          println(
            "entering method: innerLoop"
            + "\nthis: " + this
            + "\nerql: " + erql
            + "\nclient: " + client
            + "\nserver: " + server
            + "\nnode: " + node            
            + "\nrspLabelCtor: " + rspLabelCtor
          )
            reset { 
              for( e <- client.subscribe( erql ) ) {
                e match {
                  case Some( boundRsrc@BFactoryCommLink.mTT.RBoundAList( Some( BFactoryCommLink.mTT.Ground( expr ) ), subst ) ) => {
                    BasicLogService.tweet(
                      "method: innerLoop"
                      + "\n completed client.subscribe "
                      + "\nthis: " + this
                      + "\nerql: " + erql
                      + "\nclient: " + client
                      + "\nserver: " + server
                      + "\nnode: " + node
                      + "\n-----------------------------------------"
                      + "\ne: " + e
                    )
                    println(
                      "method: innerLoop"
                      + "\n completed client.subscribe "
                      + "\nthis: " + this
                      + "\nerql: " + erql
                      + "\nclient: " + client
                      + "\nserver: " + server
                      + "\nnode: " + node
                      + "\n-----------------------------------------"
                      + "\ne: " + e
                    )
                    for( map <- boundRsrc.sbst; CnxnCtxtLeaf( Left( sessionId ) ) <- map.get( "SessionId" ) ) {
                      val erspl : CnxnCtxtLabel[String,String,String] = rspLabelCtor( sessionId )
                      
                      val forward : Option[mTT.Resource] => Unit =
                        {
                          ( optRsrc : Option[mTT.Resource] ) => {
                            BasicLogService.tweet(
                              ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                              + "\nBFactory.scala:2265 forward(" + optRsrc + ")"
                              + "\n------------------------------------------------------------------"
                              + "\n defined in method innerLoop"
                              + "\n passed to and called in evaluateExpression"
                              + "\nerql: " + erql
                              + "\nserver: " + server                              
                              + "\n------------------------------------------------------------------"
                              + "\n erspl: " + erspl
                              + "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                            )
                            reset {
                              server.publish(
                                erspl,
                                BFactoryCommLink.mTT.Ground(
                                  optRsrc match {
                                    case None => {                                
                                      ConcreteBFactHL.Noop
                                    }
                                    case Some( mTT.Ground( v ) ) => {
                                      v
                                    }
                                    case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
                                      v
                                    }
                                  }
                                )
                              )       
                            }
                          }
                        }
                      
                      evaluateExpression( node, dslNode )( expr )( forward )
                    }             
                  }
                  case Some( boundRsrc@BFactoryCommLink.mTT.RBoundHM( Some( BFactoryCommLink.mTT.Ground( expr ) ), subst ) ) => {
                    BasicLogService.tweet(
                      "method: innerLoop"
                      + "\n case rsrc type: BFactoryCommLink.mTT.RBoundHM"
                      + "\n completed client.subscribe "
                      + "\nthis: " + this
                      + "\nerql: " + erql
                      + "\nclient: " + client
                      + "\nserver: " + server
                      + "\nnode: " + node
                      + "\n-----------------------------------------"
                      + "\ne: " + e
                    )
                    println(
                      "method: innerLoop"
                      + "\n case rsrc type: BFactoryCommLink.mTT.RBoundHM"
                      + "\n completed client.subscribe "
                      + "\nthis: " + this
                      + "\nerql: " + erql
                      + "\nclient: " + client
                      + "\nserver: " + server
                      + "\nnode: " + node
                      + "\n-----------------------------------------"
                      + "\ne: " + e
                    )
                    for( map <- boundRsrc.sbst; CnxnCtxtLeaf( Left( sessionId ) ) <- map.get( "SessionId" ) ) {
                      val erspl : CnxnCtxtLabel[String,String,String] = rspLabelCtor( sessionId )
                      
                      val forward : Option[mTT.Resource] => Unit =
                        {
                          ( optRsrc : Option[mTT.Resource] ) => {
                            BasicLogService.tweet(
                              ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                              + "\nBFactory.scala:2306 forward(" + optRsrc + ")"
                              + "\n------------------------------------------------------------------"
                              + "\n defined in method innerLoop"
                              + "\n passed to and called in evaluateExpression"
                              + "\nerql: " + erql
                              + "\nserver: " + server                              
                              + "\n------------------------------------------------------------------"
                              + "\n erspl: " + erspl
                              + "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                            )
                            reset {
                              server.publish(
                                erspl,
                                BFactoryCommLink.mTT.Ground(
                                  optRsrc match {
                                    case None => {                                
                                      ConcreteBFactHL.Noop
                                    }
                                    case Some( mTT.Ground( v ) ) => {
                                      v
                                    }
                                    case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
                                      v
                                    }
                                  }
                                )
                              )       
                            }
                          }
                        }
                      
                      evaluateExpression( node, dslNode )( expr )( forward )
                    }             
                  }
                  case Some( rsrc ) => {
                    rsrc match {
                      case boundRsrc@BFactoryCommLink.mTT.RBoundHM( innerOptRsrc, subst ) => {
                        BasicLogService.tweet(
                          "method: innerLoop"
                          + "\n case rsrc type: BFactoryCommLink.mTT.RBoundHM"
                          + "\n completed client.subscribe "
                          + "\nthis: " + this
                          + "\nerql: " + erql
                          + "\nclient: " + client
                          + "\nserver: " + server
                          + "\nnode: " + node
                          + "\n-----------------------------------------"
                          + "\ne: " + e
                        )
                        println(
                          "method: innerLoop"
                          + "\n case rsrc type: BFactoryCommLink.mTT.RBoundHM"
                          + "\n completed client.subscribe "
                          + "\nthis: " + this
                          + "\nerql: " + erql
                          + "\nclient: " + client
                          + "\nserver: " + server
                          + "\nnode: " + node
                          + "\n-----------------------------------------"
                          + "\ne: " + e
                        )
                        innerOptRsrc match {
                          case Some( BFactoryCommLink.mTT.Ground( expr ) ) => {
                            for( map <- boundRsrc.sbst; CnxnCtxtLeaf( Left( sessionId ) ) <- map.get( "SessionId" ) ) {
                              val erspl : CnxnCtxtLabel[String,String,String] = rspLabelCtor( sessionId )
                              
                              val forward : Option[mTT.Resource] => Unit =
                                {
                                  ( optRsrc : Option[mTT.Resource] ) => {
                                    BasicLogService.tweet(
                                      ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                                      + "\nBFactory.scala:2351 forward(" + optRsrc + ")"
                                      + "\n------------------------------------------------------------------"
                                      + "\n defined in method innerLoop"
                                      + "\n passed to and called in evaluateExpression"
                                      + "\nerql: " + erql
                                      + "\nserver: " + server                              
                                      + "\n------------------------------------------------------------------"
                                      + "\n erspl: " + erspl
                                      + "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                                    )
                                    reset {
                                      server.publish(
                                        erspl,
                                        BFactoryCommLink.mTT.Ground(
                                          optRsrc match {
                                            case None => {                                
                                              ConcreteBFactHL.Noop
                                            }
                                            case Some( mTT.Ground( v ) ) => {
                                              v
                                            }
                                            case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
                                              v
                                            }
                                          }
                                        )
                                      )       
                                    }
                                  }
                                }                        
                              evaluateExpression( node, dslNode )( expr )( forward )
                            }
                          }
                          case Some( innerRrsc ) => {
                            BasicLogService.tweet(
                              "method: innerLoop"
                              + "\n case unexpected inner rsrc type: " + innerRrsc
                              + "\ninner rsrc type: " + innerRrsc.getClass
                              + "\n completed client.subscribe "
                              + "\nthis: " + this
                              + "\nerql: " + erql
                              + "\nclient: " + client
                              + "\nserver: " + server
                              + "\nnode: " + node
                              + "\n-----------------------------------------"
                              + "\ne: " + e
                            )
                          }
                        }                      
                      }
                      case _ => {
                        BasicLogService.tweet(
                          "method: innerLoop"
                          + "\n case unexpected rsrc type: " + rsrc
                          + "\ninner rsrc type: " + rsrc.getClass
                          + "\n completed client.subscribe "
                          + "\nthis: " + this
                          + "\nerql: " + erql
                          + "\nclient: " + client
                          + "\nserver: " + server
                          + "\nnode: " + node
                          + "\n-----------------------------------------"
                          + "\ne: " + e
                        )
                      }
                    }             
                  }
                  case None => {
                    BasicLogService.tweet( "server loop waiting." )
                  }
                  case _ => {
                    BasicLogService.tweet(
                      "method: innerLoop"
                      + "\n rsrc not handled: " + e
                      + "\n completed client.subscribe "
                      + "\nthis: " + this
                      + "\nerql: " + erql
                      + "\nclient: " + client
                      + "\nserver: " + server
                      + "\nnode: " + node
                    )
                  }
                }
              }
            }
        }

        def messageProcessorLoop(
          erql : CnxnCtxtLabel[String,String,String],
          node : StdEvalChannel,
          dslNode : com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.StdEvalChannel,
          rspLabelCtor : String => CnxnCtxtLabel[String,String,String],
          useBiLink : Option[Boolean] = None,
          flip : Boolean = false
        ) : Unit = {
          val ( client, server ) = 
            useBiLink match {
              case Some( true ) => {
                BFactoryCommLinkCtor.stdBiLink()              
              }
              case Some( false ) => {
                val ( client, server ) = BFactoryCommLinkCtor.stdBiLink()
                ( server, client )
              }
              case None => {          
                val link = BFactoryCommLinkCtor.stdLink()( flip )
                ( link, link )
              }
            }
          innerLoop( erql, client, server, node, dslNode, rspLabelCtor )
        }

        def lateMessageProcessorLoop(
          erql : CnxnCtxtLabel[String,String,String],
          node : String,
          dslNode : String,
          rspLabelCtor : String => CnxnCtxtLabel[String,String,String],
          useBiLink : Option[Boolean] = None,
          flip : Boolean = false
        ) : Unit = {
          val ( client, server ) = 
            useBiLink match {
              case Some( true ) => {
                BFactoryCommLinkCtor.stdBiLink()              
              }
              case Some( false ) => {
                val ( client, server ) = BFactoryCommLinkCtor.stdBiLink()
                ( server, client )
              }
              case None => {          
                val link = BFactoryCommLinkCtor.stdLink()( flip )
                ( link, link )
              }
            }
          innerLoop( erql, client, server, node, dslNode, rspLabelCtor )
        }

        def go( derefNodeEarly : Boolean = false ) : Unit = {
          throw new Exception( "attempting to run an abstract MessageProcessor" )
        }
      }      

      class MsgProcessorVals(
        @transient
        override val erql : CnxnCtxtLabel[String,String,String],
        @transient
        override val rspLabelCtor : String => CnxnCtxtLabel[String,String,String],
        override val useBiLink : Option[Boolean] = None,
        override val flip : Boolean = false
      ) extends MessageProcessorElements with Serializable {
        def this() = { this( null, null, None, false ) }
      }

      object MsgProcessorVals extends Serializable {
        def apply(
          erql : CnxnCtxtLabel[String,String,String],
          rspLabelCtor : String => CnxnCtxtLabel[String,String,String],
          useBiLink : Option[Boolean] = None,
          flip : Boolean = false
        ) : MsgProcessorVals = {
          new MsgProcessorVals( erql, rspLabelCtor, useBiLink, flip )
        }
        def unapply(
          mp : MsgProcessorVals
        ) : Option[
             (
               CnxnCtxtLabel[String,String,String],               
               String =>CnxnCtxtLabel[String,String,String],
               Option[Boolean],
               Boolean
             )
        ]
        = {
          Some( ( mp.erql, mp.rspLabelCtor, mp.useBiLink, mp.flip ) )
        }
      }

      case class MsgProcessor(
        @transient
        val node : StdEvalChannel,        
        @transient
        val dslNode : com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.StdEvalChannel,
        @transient
        override val erql : CnxnCtxtLabel[String,String,String],
        @transient
        override val rspLabelCtor : String => CnxnCtxtLabel[String,String,String],
        override val useBiLink : Option[Boolean] = None,
        override val flip : Boolean = false
      ) extends MsgProcessorVals(
        erql, rspLabelCtor, useBiLink, flip
      ) with MessageProcessor with Serializable {
        def this() = { this( null, null, null, null, None, false ) }
        override def go( derefNodeEarly : Boolean = true ) : Unit = {
          if ( derefNodeEarly ) {
            messageProcessorLoop( erql, node, dslNode, rspLabelCtor, useBiLink, flip )
          }
          else {
            BasicLogService.tweet( "warning: derefing node early anyway"  )
            messageProcessorLoop( erql, node, dslNode, rspLabelCtor, useBiLink, flip )
          }
        }
      }

/*
      object MsgProcessor extends Serializable {
        def apply(
          node : StdEvalChannel,
          erql : CnxnCtxtLabel[String,String,String],
          rspLabelCtor : String => CnxnCtxtLabel[String,String,String],
          useBiLink : Option[Boolean] = None,
          flip : Boolean = false
        ) : MsgProcessor = {
          new MsgProcessor( node, erql, rspLabelCtor, useBiLink, flip )
        }
        def unapply(
          mp : MsgProcessor
        ) : Option[
             (
               StdEvalChannel,
               CnxnCtxtLabel[String,String,String],               
               String =>CnxnCtxtLabel[String,String,String],
               Option[Boolean],
               Boolean
             )
        ]
        = {
          Some( ( mp.node, mp.erql, mp.rspLabelCtor, mp.useBiLink, mp.flip ) )
        }
      }
 */

      case class IndirectMsgProcessor(
        val node : String,
        val dslNode : String,
        @transient
        override val erql : CnxnCtxtLabel[String,String,String],
        @transient
        override val rspLabelCtor : String => CnxnCtxtLabel[String,String,String],
        override val useBiLink : Option[Boolean] = None,
        override val flip : Boolean = false
      ) extends MsgProcessorVals(
        erql, rspLabelCtor, useBiLink, flip
      ) with MessageProcessor with Serializable {
        def this() = { this( null, null, null, null, None, false ) }
        override def go( derefNodeEarly : Boolean = false ) : Unit = {
          if ( derefNodeEarly ) {            
            for(
              n <- EvalNodeMapper.get( node );
              dslN <- com.biosimilarity.evaluator.distribution.diesel.EvalNodeMapper.get( dslNode )
            ) {
              messageProcessorLoop( erql, n, dslN, rspLabelCtor, useBiLink, flip )
            }
          }
          else {
            lateMessageProcessorLoop( erql, node, dslNode, rspLabelCtor, useBiLink, flip )
          }
        }
      }

/*
      object IndirectMsgProcessor extends Serializable {
        def apply(
          node : String,
          erql : CnxnCtxtLabel[String,String,String],
          rspLabelCtor : String => CnxnCtxtLabel[String,String,String],
          useBiLink : Option[Boolean] = None,
          flip : Boolean = false
        ) : IndirectMsgProcessor = {
          new IndirectMsgProcessor( node, erql, rspLabelCtor, useBiLink, flip )
        }
        def unapply(
          mp : IndirectMsgProcessor
        ) : Option[
             (
               String,
               CnxnCtxtLabel[String,String,String],
               String =>CnxnCtxtLabel[String,String,String],
               Option[Boolean],
               Boolean
             )
        ]
        = {
          Some( ( mp.node, mp.erql, mp.rspLabelCtor, mp.useBiLink, mp.flip ) )
        }
      }
*/

      case class MsgProcessorBlock(
        @transient
        override val self : List[MessageProcessor]
      ) extends scala.collection.SeqProxy[MessageProcessor] {
        def go() { for ( mp <- self ) { mp.go() } }
      }

      def adminLooper(
        node : StdEvalChannel,
        dslNode : com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.StdEvalChannel,
        useBiLink : Option[Boolean] = None,
        flip : Boolean = false
      ) : MsgProcessor = {
        MsgProcessor(
          node,
          dslNode,
          BFactoryCommLinkCtor.ExchangeLabels.adminRequestLabel()( Right[String,String]( "SessionId" ) ).getOrElse( 
            throw new Exception( "error making evalRequestLabel" )
          ),
          ( sessionId : String ) => {
            BFactoryCommLinkCtor.ExchangeLabels.adminResponseLabel()(
              Left[String,String]( sessionId )
            ).getOrElse( throw new Exception( "unable to make evaResponseLabel" ) )
          },
          useBiLink,
          flip
        )
      }

      def indirectAdminLooper(
        node : String,
        dslNode : String,
        useBiLink : Option[Boolean] = None,
        flip : Boolean = false
      ) : IndirectMsgProcessor = {
        IndirectMsgProcessor(
          node,
          dslNode,
          BFactoryCommLinkCtor.ExchangeLabels.adminRequestLabel()( Right[String,String]( "SessionId" ) ).getOrElse( 
            throw new Exception( "error making evalRequestLabel" )
          ),
          ( sessionId : String ) => {
            BFactoryCommLinkCtor.ExchangeLabels.adminResponseLabel()(
              Left[String,String]( sessionId )
            ).getOrElse( throw new Exception( "unable to make evaResponseLabel" ) )
          },
          useBiLink,
          flip
        )
      }

      def evalLooper(
        node : StdEvalChannel,
        dslNode : com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.StdEvalChannel,
        useBiLink : Option[Boolean] = None,
        flip : Boolean = false
      ) : MsgProcessor = {
        MsgProcessor(
          node,
          dslNode,
          BFactoryCommLinkCtor.ExchangeLabels.evalRequestLabel()( Right[String,String]( "SessionId" ) ).getOrElse( 
              throw new Exception( "error making evalRequestLabel" )
            ),
          ( sessionId : String ) => {
            BFactoryCommLinkCtor.ExchangeLabels.evalResponseLabel()(
              Left[String,String]( sessionId )
            ).getOrElse( throw new Exception( "unable to make evaResponseLabel" ) )
          },
          useBiLink,
          flip
        )
      }

      def indirectEvalLooper(
        node : String,
        dslNode : String,
        useBiLink : Option[Boolean] = None,
        flip : Boolean = false
      ) : IndirectMsgProcessor = {
        IndirectMsgProcessor(
          node,
          dslNode,
          BFactoryCommLinkCtor.ExchangeLabels.evalRequestLabel()( Right[String,String]( "SessionId" ) ).getOrElse( 
              throw new Exception( "error making evalRequestLabel" )
            ),
          ( sessionId : String ) => {
            BFactoryCommLinkCtor.ExchangeLabels.evalResponseLabel()(
              Left[String,String]( sessionId )
            ).getOrElse( throw new Exception( "unable to make evaResponseLabel" ) )
          },
          useBiLink,
          flip
        )
      }

      def stdLooper(
        //node : StdEvalChannel = agent( "/bFactoryProtocol" ),
        node : StdEvalChannel = dslEvaluatorAgent( ),
        dslNode : com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.StdEvalChannel = com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.dslEvaluatorAgent( ),
        useBiLink : Option[Boolean] = None,
        flip : Boolean = false
      ) : MsgProcessorBlock = {
        MsgProcessorBlock(
          List[MsgProcessor](
            adminLooper( node, dslNode, useBiLink, flip ),
            evalLooper( node, dslNode, useBiLink, flip )
          )
        )
      }
      
      def indirectStdLooper(
        node : String,
        dslNode : String,
        useBiLink : Option[Boolean] = None,
        flip : Boolean = false
      ) : MsgProcessorBlock = {
        MsgProcessorBlock(
          List[MessageProcessor](
            indirectAdminLooper( node, dslNode, useBiLink, flip ),
            indirectEvalLooper( node, dslNode, useBiLink, flip )
          )
        )
      }
      
      def mkNodeEvaluator(
        node : String,
        dslNode : String
      ) : BehaviorService = {
        new BehaviorService with Serializable {
          type Rsrc = mTT.Resource
          def commenceInstanceHandler(
            n : StdEvalChannel,
            dslN : com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.StdEvalChannel
          )(
            bdc : ConcreteBFactHL.Cnxn,
            bdl : ConcreteBFactHL.Label,
            cnxns : Seq[ConcreteBFactHL.Cnxn],
            filters : Seq[ConcreteBFactHL.Label],
            handler : Option[Rsrc] => Unit
          ) = {
            BasicLogService.tweet(
              "method: commenceInstanceHandler"
              + "\nin ConcreteBFactHL.CommenceInstance case "
              + "\nthis: " + this
              + "\nn: " + n
              + "\ndslN: " + dslN
              + "\n-----------------------------------------"
              + "\nbehaviorDefinitionCnxn: " + bdc
              + "\nbehaviorDefinitionLabel: " + bdl
              + "\ncnxns: " + cnxns
              + "\nfilters: " + filters                
              + "\nhandler: " + handler
            )
            
            val agntCnxn : acT.AgentCnxn =
              new acT.AgentCnxn( bdc.src, bdc.label.toString, bdc.trgt )
            reset {
              
              BasicLogService.tweet(
                "method: commenceInstanceHandler"
                + "\n calling node.fetch "
                + "\nthis: " + this
                + "\nn: " + n
                + "\ndslN: " + dslN
                + "\n-----------------------------------------"
                + "\nbehaviorDefinitionCnxn: " + agntCnxn
                + "\nbehaviorDefinitionLabel: " + bdl
              )

              def handleRsrc( optRsrc : Option[Rsrc], behavior : String ) : Unit = {
                try {
                  BasicLogService.tweet(
                    "method: commenceInstanceHandler"
                    + "\n instantiating instance & finding entry point"
                    + "\nthis: " + this
                    + "\nn: " + n
                    + "\ndslN: " + dslN
                    + "\n-----------------------------------------"
                    + "\nbehaviorDefinitionCnxn: " + agntCnxn
                    + "\nbehaviorDefinitionLabel: " + bdl
                    + "\nbehavior: " + behavior
                    + "\ncnxns: " + cnxns
                    + "\nfilters: " + filters
                    + "\nhandler: " + handler
                  )
                  BFactoryMirror.instanceEntryPoint( behavior, "run" ) match {
                    case Left( entryPointM ) => {
                      spawn {
                        val instanceID = UUID.randomUUID
                        val instanceLabel =
                          StorageLabels.instanceStorageLabel()( Left[String,String]( instanceID.toString ) )
                        entryPointM( dslN, cnxns, filters )
                        handler( 
                          Some(
                            mTT.Ground(
                              ConcreteBFactHL.InstanceRunning(
                                bdc,
                                instanceLabel
                              )
                            )
                          )
                        )
                        
                      }
                    }
                    case Right( e ) => {
                      handler( 
                        Some(
                          mTT.Ground(
                            ConcreteBFactHL.InstanceNotRunning(
                              bdc,
                              bdl,
                              "instantiation failed" + e
                            )
                          )
                        )
                      )
                    }
                  }
                }
                catch {
                  case e : Throwable => {
                    handler( 
                      Some(
                        mTT.Ground(
                          ConcreteBFactHL.InstanceNotRunning(
                            bdc,
                            bdl,
                            "instantiation failed" + e
                          )
                        )
                      )
                    )
                  }
                }                      
              }
              
              for( e <- n.fetch( agntCnxn )( bdl ) ) {
                
                BasicLogService.tweet(
                  "method: commenceInstanceHandler"
                  + "\n returned from node.fetch "
                  + "\nthis: " + this
                  + "\nn: " + n
                  + "\ndslN: " + dslN
                  + "\n-----------------------------------------"
                  + "\nagntCnxn: " + agntCnxn
                  + "\nbehaviorDefinitionLabel: " + bdl
                  + "\ne: " + e
                  + "\nhandler: " + handler
                )
                
                e match {
                  case Some( mTT.Ground( ConcreteBFactHL.WrappedBehaviorIdentifier( behavior ) ) ) => {
                    handleRsrc( e, behavior )
                  }
                  case Some( mTT.RBoundAList( Some( mTT.Ground( ConcreteBFactHL.WrappedBehaviorIdentifier( behavior ))), _ ) ) => {           
                    handleRsrc( e, behavior )
                  }
                  case _ => {
                    handler( 
                      Some(
                        mTT.Ground(
                          ConcreteBFactHL.InstanceNotRunning(
                            bdc,
                            bdl,
                            "unexpected behavior record format: " + e
                          )
                        )
                      )
                    )
                  }
                }                                    
              }
            }
          }
          def mapBehavior[Value](
            cnxn : ConcreteBFactHL.Cnxn,
            label : CnxnCtxtLabel[String,String,String],
            behavior : String,
            onMap : Option[Rsrc] => Unit        
          ) : Unit = {
            for (
              n <- EvalNodeMapper.get( node );
              dslN <- com.biosimilarity.evaluator.distribution.diesel.EvalNodeMapper.get( dslNode )
            ) {
              BasicLogService.tweet(
                "method: mapBehavior"
                + "\nin ConcreteBFactHL.MapBehavior case "
                + "\nthis: " + this
                + "\nnode: " + node
                + "\nonMap: " + onMap
                + "\n-----------------------------------------"
                + "\ncnxn: " + cnxn
                + "\nlabel: " + label
                + "\nbehavior: " + behavior
              )
              
              val agntCnxn : acT.AgentCnxn =
                new acT.AgentCnxn( cnxn.src, cnxn.label.toString, cnxn.trgt )
              reset {
                
                BasicLogService.tweet(
                  "method: mapBehavior"
                  + "\n calling node.publish "
                  + "\nthis: " + this
                  + "\nnode: " + node
                  + "\nonMap: " + onMap
                  + "\n-----------------------------------------"
                  + "\nagntCnxn: " + agntCnxn
                  + "\nlabel: " + label
                  + "\nbehavior: " + behavior
                )
                
                try {
                  n.publish( agntCnxn )( label, mTT.Ground( ConcreteBFactHL.WrappedBehaviorIdentifier( behavior ) ) )
                  onMap( 
                    Some(
                      mTT.Ground(
                        ConcreteBFactHL.Noop
                      )
                    )
                  )
                } 
                catch {
                  case e : Exception => {
                    BasicLogService.tweet(
                      "method: mapBehavior"
                      + "\n ---> node.publish caused an exception <--- "
                      + "\nthis: " + this
                      + "\nnode: " + node
                      + "\nonMap: " + onMap
                      + "\n-----------------------------------------"
                      + "\nagntCnxn: " + agntCnxn
                      + "\nlabel: " + label
                      + "\nbehavior: " + behavior
                    )
                    BasicLogService.tweetTrace( e )
                  }
                }
              }
            }
          }
          def commenceInstance(
            behaviorDefinitionCnxn : ConcreteBFactHL.Cnxn,
            behaviorDefinitionLabel : CnxnCtxtLabel[String,String,String],
            cnxns : Seq[ConcreteBFactHL.Cnxn],
            filters : Seq[CnxnCtxtLabel[String,String,String]],
            onCommencement : Option[Rsrc] => Unit
          ) : Unit = {
            for (
              n <- EvalNodeMapper.get( node );
              dslN <- com.biosimilarity.evaluator.distribution.diesel.EvalNodeMapper.get( dslNode )
            ) {
              commenceInstanceHandler( n, dslN )(
                behaviorDefinitionCnxn,
                behaviorDefinitionLabel,
                cnxns,
                filters,
                onCommencement
              )
            }
          }
          def completeInstance(
            behaviorInstanceCnxn : ConcreteBFactHL.Cnxn,
            behaviorInstanceLabel : CnxnCtxtLabel[String,String,String],
            onCompletion : Option[Rsrc] => Unit
          ) : Unit = {
            throw new Exception( "method: completeInstance -- not yet implemented" )
          }
        }
      }
    }    
  }

  object CommsLinkMapper extends MapProxy[String,BFactoryCommLinkCtor.StdEvaluationRequestChannel] {
    @transient
    override val self = new HashMap[String,BFactoryCommLinkCtor.StdEvaluationRequestChannel]()
  }

  object EvalNodeMapper extends MapProxy[String,BFactoryEngineCtor.StdEvalChannel] {
    @transient
    override val self = new HashMap[String,BFactoryEngineCtor.StdEvalChannel]()
  }

  object Server extends Serializable {
    lazy val helpMsg = 
      (
        "-help -- this message\n"
        + "config=<fileName>\n" 
      )
    def processArgs(
      args : Array[String]
    ) : HashMap[String,String] = {
      val map = new HashMap[String,String]()
      for( arg <- args ) {
        val argNVal = arg.split( "=" )
        if ( argNVal.size > 1 ) {
          ( argNVal( 0 ), argNVal( 1 ) ) match {
            case ( "config", file ) => {
              map += ( "config" -> file )
            }
          }
        }
        else {
          arg match {
            case "-help" => {
              println( helpMsg )
            }
            case _ => {
              println( "unrecognized arg: " + arg )
              println( helpMsg )
            }
          }       
        }
      }
      map
    }

    @transient
    var _engine : Option[BFactoryEngineCtor.BFactoryEngine] = None
    def engine( s : Option[String] = Some( "eval.conf" ) ) : BFactoryEngineCtor.BFactoryEngine = {
      _engine match {
        case Some( e ) => e
        case None => {
          val e = new BFactoryEngineCtor.BFactoryEngine( s )
          _engine = Some( e )
          e
        }
      }
    }

    @transient
    var _looper : Option[BFactoryEngineCtor.BFactoryEngine#MsgProcessorBlock] = None
    def looper(
      e : BFactoryEngineCtor.BFactoryEngine = engine( None )
    ) : BFactoryEngineCtor.BFactoryEngine#MsgProcessorBlock = {
      _looper match {
        case Some( mpb ) => mpb
        case None => {
          val nodeId = UUID.randomUUID()
          val nodeKey = nodeId.toString
          val dslNodeId = UUID.randomUUID()
          val dslNodeKey = nodeId.toString
          //EvalNodeMapper += ( nodeKey -> BFactoryEngineCtor.agent( "/bFactoryProtocol" ) )
          EvalNodeMapper += ( nodeKey -> BFactoryEngineCtor.dslEvaluatorAgent( ) )
          com.biosimilarity.evaluator.distribution.diesel.EvalNodeMapper += ( dslNodeKey -> com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.dslEvaluatorAgent( ) )
          val mpb = e.indirectStdLooper( nodeKey, dslNodeKey )
          _looper = Some( mpb )
          mpb
        }
      }
    }

    @transient
    var _localService : Option[BehaviorService] = None
    def localService(
      e : BFactoryEngineCtor.BFactoryEngine = engine( Some( "eval.conf" ) )
    ) : BehaviorService = {
      _localService match {
        case Some( ls ) => ls
        case None => {
          val nodeId = UUID.randomUUID()
          val nodeKey = nodeId.toString
          val dslNodeId = UUID.randomUUID()
          val dslNodeKey = nodeId.toString
          //EvalNodeMapper += ( nodeKey -> BFactoryEngineCtor.agent( "/bFactoryProtocol" ) )
          EvalNodeMapper += ( nodeKey -> BFactoryEngineCtor.dslEvaluatorAgent( ) )
          com.biosimilarity.evaluator.distribution.diesel.EvalNodeMapper += ( dslNodeKey -> com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.dslEvaluatorAgent( ) )
          val ls = e.mkNodeEvaluator( nodeKey, dslNodeKey )
          _localService = Some( ls )
          ls
        }
      }
    }
    
    def run( args : Array[String] ) : Unit = {
      @transient
      val map = processArgs( args )
      @transient
      //val e = new BFactoryEngineCtor.BFactoryEngine( map.get( "config" ) )
      val e = engine( map.get( "config" ) )
      val version = e.version
      println( "*******************************************************" )
      println( "******************** BFactory engine ********************" )
      println( "******************** Version " + version + " ********************" )
      println( "*******************************************************" )
      
      looper( e ).go()
    }

    def run( ) : Unit = {
      val a1 = new Array[String]( 1 )
      a1( 0 ) = "config=eval.conf" 
      run( a1 )
    }
  }
}
