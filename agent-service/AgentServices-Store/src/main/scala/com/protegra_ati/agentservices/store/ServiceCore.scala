// -*- mode: Scala;-*- 
// Filename:    ServiceCore.scala 
// Authors:     lgm                                                    
// Creation:    Wed Oct 10 09:37:30 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.store

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

import org.xmldb.api.base.{ Resource => XmlDbRrsc, _}
import org.xmldb.api.modules._
import org.xmldb.api._

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

package services {
  trait PlatformServiceCoreT[Namespace,Var,Tag,Value]
   extends AgentKVDBNodeScope[Namespace,Var,Tag,Value]
    with UUIDOps {    
    import SpecialKURIDefaults._
    import identityConversions._

    def seedTag : Tag 
    def seedVal : Value
    def seedKVNameSpace : Namespace
    def seedKVKNameSpace : Namespace

    type ACTypes = AgentCnxnTypes
    object TheACT extends ACTypes
    override def protoAgentCnxnTypes : ACTypes = TheACT

    type MTTypes = MonadicTermTypes[Namespace,Var,Tag,Value]
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
    lazy val aLabel = new CnxnCtxtLeaf[Namespace,Var,Tag]( Left( seedTag ) )

    object MonadicDRsrcMsgs extends RsrcMsgTypes with Serializable {
      
      @transient
      override def protoDreq : DReq = MDGetRequest( aLabel )
      @transient
      override def protoDrsp : DRsp = MDGetResponse( aLabel, seedVal )
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

    trait Endurance extends AgentPersistenceScope with Serializable {
      override type EMTypes =
	ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]
      trait StdEMTypes extends ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]
       with Serializable {
	 
	 trait PlatformServiceFactoryT
	  extends BaseAgentKVDBNodeFactoryT with AgentKVDBNodeFactoryT with Serializable {
	    type AgentCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse] = AgentKVDB[ReqBody,RspBody]
	    def mkCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
	      here : URI,
	      configFileName : Option[String]
	    ) : AgentCache[ReqBody,RspBody] = {
	      new AgentKVDB[ReqBody, RspBody](
		MURI( here ), configFileName
	      ) with Blobify with AMQPMonikerOps {		
                override def toXQSafeJSONBlob( x : java.lang.Object ) : String = {
                  new XStream( new JettisonMappedXmlDriver() ).toXML( x )
                }
                override def fromXQSafeJSONBlob( blob : String ) : java.lang.Object = {              
                  new XStream( new JettisonMappedXmlDriver() ).fromXML( blob )
                }      
		class LocalXMLDBManifest(
		  override val storeUnitStr : String,
		  @transient override val labelToNS : Option[String => Namespace],
		  @transient override val textToVar : Option[String => Var],
		  @transient override val textToTag : Option[String => Tag],
			@transient override val textToValue: Option[String => Value]
		)
		extends XMLDBManifest( database ) {
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
		  
		  def kvNameSpace : Namespace = seedKVNameSpace
		  def kvKNameSpace : Namespace = seedKVKNameSpace
		  
		  def compareNameSpace( ns1 : Namespace, ns2 : Namespace ) : Boolean = {
		    throw new Exception( "Not implemented in this service" )
		  }
		  
		  override def asStoreValue(
		    rsrc : mTT.Resource
		  ) : CnxnCtxtLeaf[Namespace,Var,String] with Factual = {
		    throw new Exception( "Not implemented in this service" )
		  }
		  
		  def asCacheValue(
		    ccl : CnxnCtxtLabel[Namespace,Var,String]
		  ) : Value = {
		    throw new Exception( "Not implemented in this service" )
		  }

			def asIndirection(key: mTT.GetRequest, value: Elem): Option[mTT.GetRequest] =
				throw new Exception("Not implemented in this service")
		  
		  override def asResource(
		    key : mTT.GetRequest, // must have the pattern to determine bindings
		    value : Elem
		  ) : emT.PlaceInstance = {
		    throw new Exception( "Not implemented in this service" )
		  }
		  
		}
		override def asCacheK(
		  ccl : CnxnCtxtLabel[Namespace,Var,String]
		) : Option[mTT.Continuation] = {
		  throw new Exception( "Not implemented in this service" )
		}
		
		override def asCacheK(
		  ltns : String => Namespace,
		  ttv : String => Var,
		  value : Elem
		) : Option[mTT.Continuation] = {
		  throw new Exception( "shouldn't be calling this version of asCacheK" )
		}
		override def persistenceManifest : Option[PersistenceManifest] = None

		def dfStoreUnitStr : String = mnkrExchange( name )
	      }
	    }
	    def ptToPt[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
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
		    throw new Exception( "method not implemented in service" )
		  }
		}
	      spawn { node.dispatchDMsgs() }
	      node
	    }
	    def ptToMany[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
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
		    throw new Exception( "method not implemented in service" )
		  }
		}
	      spawn { node.dispatchDMsgs() }
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
		    throw new Exception( "method not implemented in service" )
		  }
		}
	      spawn { node.dispatchDMsgs() }
	      node
	    }
	  }
	 
       }
      //override def protoEMTypes : EMTypes = theEMTypes
    }
  }

  // This is the smallest possible node-based service
  class PlatformServiceCore[Namespace,Var,Tag,Value](
    @transient override val seedTag : Tag,
    @transient override val seedVal : Value,
    @transient override val seedKVNameSpace : Namespace,
    @transient override val seedKVKNameSpace : Namespace
  ) extends PlatformServiceCoreT[Namespace,Var,Tag,Value]
  with Serializable {
    object Being extends Endurance {
      object theStdEMTypes extends StdEMTypes {
	object PlatformServiceFactory extends PlatformServiceFactoryT
      }
      override def protoEMTypes : EMTypes = theStdEMTypes    
    }    
  }

  class MigrationService[Namespace,Var,Tag,Value](
    @transient override val seedTag : Tag,
    @transient override val seedVal : Value,
    @transient override val seedKVNameSpace : Namespace,
    @transient override val seedKVKNameSpace : Namespace
  ) extends PlatformServiceCore[Namespace,Var,Tag,Value](
    seedTag, seedVal, seedKVNameSpace, seedKVKNameSpace
  ) {    
  }
}
