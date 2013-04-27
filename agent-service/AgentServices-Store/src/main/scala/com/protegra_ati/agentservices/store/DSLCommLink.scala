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
	  class StringMongoDBManifest(
	    override val storeUnitStr : String,
	    @transient override val labelToNS : Option[String => String],
	    @transient override val textToVar : Option[String => String],
	    @transient override val textToTag : Option[String => String]
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
	      tweet(
		"In asStoreValue on " + this + " for resource: " + rsrc
	      )
	      val storageDispatch = 
		rsrc match {
		  case k : mTT.Continuation => {
		    tweet(
		      "Resource " + rsrc + " is a continuation"
		    )
		    continuationStorageType
		  }
		  case _ => {
		    tweet(
		      "Resource " + rsrc + " is a value"
		    )
		    valueStorageType
		  }
		};
	      
	      tweet(
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
		    tweet(
		      "warning: CnxnCtxtLabel method is using XStream"
		    )
		    toXQSafeJSONBlob( rsrc )		  		  
		  }
		  case "XStream" => {
		    tweet(
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
	    ) : ConcreteHL.HLExpr = {
	      tweet(
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
		      getGV( rsrc ).getOrElse( ConcreteHL.Bottom )
		    }
		  }
		}
		case _ => {
		  //asPatternString( ccl )
		  throw new Exception( "unexpected value form: " + ccl )
		}
	      }
	    }
	    
	    override def asResource(
	      key : mTT.GetRequest, // must have the pattern to determine bindings
	      value : DBObject
	    ) : emT.PlaceInstance = {
	      // BUGBUG -- lgm : what was the rationale for the
	      // roundtrip?
	      /*
	       val cclKey =
	       xmlIfier.fromXML( ltns, ttv, ttt )(
	       xmlIfier.asXML( key )
	       ) match {
	       case Some( cclX ) => cclX
	       case _ => throw new Exception( "xml roundtrip failed " + key )
 	       }
	       */
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
	      CnxnMongoObjectifier.fromMongoObject( value )( ltns, ttv, ttt ) match {
		case CnxnCtxtBranch( ns, CnxnCtxtBranch( kNs, k :: Nil ) :: CnxnCtxtBranch( vNs, v :: Nil ) :: Nil ) => {
		  tweet( " ****************************** " )
		  tweet( "vNs: " + vNs )
		  tweet( "v: " + v )
		  tweet( " ****************************** " )
		  matchMap( key, k ) match {
		    case Some( soln ) => {
		      if ( compareNameSpace( ns, kvNameSpace ) ) {
			emT.PlaceInstance(
			  k,
			  Left[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]](
			    mTT.Ground(
			      asCacheValue(
				new CnxnCtxtBranch[String,String,String](
				  "string",
				  v :: Nil
				)
			      )
			    )
			  ),
			  // BUGBUG -- lgm : why can't the compiler determine
			  // that this cast is not necessary?
			  theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
			)
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
		      tweet( "Unexpected matchMap failure: " + key + " " + k )
		      throw new Exception( "matchMap failure " + key + " " + k )
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
	    tweet(
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
		      // tweet(
		      // 		      "warning: CnxnCtxtLabel method is using XStream"
		      // 		    )
		      fromXQSafeJSONBlob( rv )
		    }
		    case "XStream" => {
		      fromXQSafeJSONBlob( rv )
		    }
		    case "Base64" => {
		      val data : Array[Byte] = Base64Coder.decode( rv )
		      val ois : ObjectInputStream =
			new ObjectInputStream( new ByteArrayInputStream(  data ) )
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
	    val kvdb = this;
	    Some(
	      new StringMongoDBManifest( dfStoreUnitStr, sid, sid, sid ) {
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

object DSLCommLinkCtor extends Serializable {
  import DSLCommLink._   
  import Being._
  import PersistedKVDBNodeFactory._

  object ExchangeLabels extends CnxnString[String,String,String] {
    def evalRequestLabel( majorVersion : String, minorVersion : String, sessionId : String ) = {
      fromTermString(
	"evalRequestLabel( majorVersion( \""
	+ majorVersion
	+ "\" ), minorVersion( \""
	+ minorVersion
	+ "\"), sessionId( \""
	+ sessionId
	+ "\" ) )"
      )
    }
    def evalResponseLabel( majorVersion : String, minorVersion : String, sessionId : String ) = {
      fromTermString(
	"evalResponseLabel( majorVersion( \""
	+ majorVersion
	+ "\" ), minorVersion( \""
	+ minorVersion
	+ "\"), sessionId( \""
	+ sessionId
	+ "\" ) )"
      )
    }
  }
  
  implicit val retTwist : Boolean = false
  def setup[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
    localHost : String, localPort : Int,
    remoteHost : String, remotePort : Int
  )(
    implicit returnTwist : Boolean
  ) : Either[Being.PersistedMonadicKVDBNode[ReqBody,RspBody],(Being.PersistedMonadicKVDBNode[ReqBody, RspBody],Being.PersistedMonadicKVDBNode[ReqBody, RspBody])] = {
    val ( localExchange, remoteExchange ) = 
      if ( localHost.equals( remoteHost ) && ( localPort == remotePort ) ) {
	( "/DSLExecProtocolLocal", "/DSLExecProtocolRemote" )	  
      }
      else {
	( "/DSLExecProtocol", "/DSLExecProtocol" )	  
      }
    
    if ( returnTwist ) {
      Right[Being.PersistedMonadicKVDBNode[ReqBody,RspBody],(Being.PersistedMonadicKVDBNode[ReqBody, RspBody],Being.PersistedMonadicKVDBNode[ReqBody, RspBody])](
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
      Left[Being.PersistedMonadicKVDBNode[ReqBody, RspBody],(Being.PersistedMonadicKVDBNode[ReqBody, RspBody],Being.PersistedMonadicKVDBNode[ReqBody, RspBody])](
	ptToPt(
	  new URI( "agent", null, localHost, localPort, localExchange, null, null ),
	  new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
	)
      )
    }
  }

  def link[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
  ) : Being.PersistedMonadicKVDBNode[ReqBody,RspBody] = {
    val Right( ( client, server ) ) = 
      setup[ReqBody,RspBody]( "localhost", 5672, "localhost", 5672 )( true )
    client
  }	 
  
}
