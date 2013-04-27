// -*- mode: Scala;-*- 
// Filename:    Diesel.scala 
// Authors:     lgm                                                    
// Creation:    Sat Apr 27 00:25:52 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

import com.biosimilarity.evaluator.dsl._

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

package diesel {
  import scala.xml._
  import scala.xml.XML._
  import scala.collection.mutable.Buffer
  import scala.collection.mutable.ListBuffer

  object DieselEngineScope
	 extends AgentKVDBMongoNodeScope[String,String,String,ConcreteHL.HLExpr]
	 with UUIDOps
	 //with BulkCollectDImport
	 with Serializable
  {
    import SpecialKURIDefaults._
    import identityConversions._

    type ACTypes = AgentCnxnTypes
    object TheACT extends ACTypes
    override def protoAgentCnxnTypes : ACTypes = TheACT

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
	     with WireTap with Journalist
	     with Serializable {	  	       
	type AgentCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse] = AgentKVDB[ReqBody,RspBody]
        //type AgentNode[Rq <: PersistedKVDBNodeRequest, Rs <: PersistedKVDBNodeResponse] = AgentKVDBNode[Rq,Rs]

	override def tap [A] ( fact : A ) : Unit = {
	  reportage( fact )
	}

	override def mkCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( 
	  here : URI,
	  configFileName : Option[String]
	) : AgentCache[ReqBody,RspBody] = {
	  new AgentKVDB[ReqBody, RspBody](
	    MURI( here ),
	    configFileName
	  ) with Blobify with AMQPMonikerOps {		
	    class StringMongoDBManifest(
	      override val storeUnitStr : String,
	      @transient override val labelToNS : Option[String => String],
	      @transient override val textToVar : Option[String => String],
	      @transient override val textToTag : Option[String => String]
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
	      tweet(
		(
		  "AgentKVDB : "
		  + "\nthis: " + this
		  + "\n method : persistenceManifest "
		)
	      )
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
		tweet(
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
		  class StringMongoDBManifest(
		    override val storeUnitStr : String,
		    @transient override val labelToNS : Option[String => String],
		    @transient override val textToVar : Option[String => String],
		    @transient override val textToTag : Option[String => String]
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
		    tweet(
		      (
			"HashAgentKVDB : "
			+ "\nthis: " + this
			+ "\n method : persistenceManifest "
		      )
		    )
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
		tweet(
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
		  class StringMongoDBManifest(
		    override val storeUnitStr : String,
		    @transient override val labelToNS : Option[String => String],
		    @transient override val textToVar : Option[String => String],
		    @transient override val textToTag : Option[String => String]
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
		    tweet(
		      (
			"HashAgentKVDB : "
			+ "\nthis: " + this
			+ "\n method : persistenceManifest "
		      )
		    )
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
	    }
	  spawn {
	    println( "initiating dispatch on " + node )
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
		tweet(
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
		  class StringMongoDBManifest(
		    override val storeUnitStr : String,
		    @transient override val labelToNS : Option[String => String],
		    @transient override val textToVar : Option[String => String],
		    @transient override val textToTag : Option[String => String]
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
		    tweet(
		      (
			"HashAgentKVDB : "
			+ "\nthis: " + this
			+ "\n method : persistenceManifest "
		      )
		    )
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
	    }
	  spawn {
	    println( "initiating dispatch on " + node )
	    node.dispatchDMsgs()
	  }
	  node
	}
      }
    }

    import java.util.regex.{Pattern => RegexPtn, Matcher => RegexMatcher}
    
    def createExchange[Rsrc]() : MonadicTupleSpace[String,String,Rsrc] = {
      new MonadicTupleSpace[String,String,Rsrc] with WireTap with Journalist
		  with ConfiggyReporting
		  with ConfiguredJournal
		  with ConfigurationTrampoline {

		    override type Substitution = IdentitySubstitution

		    override val theMeetingPlace = new HashMap[String,Rsrc]()
		    override val theChannels = new HashMap[String,Rsrc]()
		    override val theWaiters = new HashMap[String,List[RK]]()
		    override val theSubscriptions = new HashMap[String,List[RK]]()
		    
		    override def tap [A] ( fact : A ) : Unit = {
		      reportage( fact )
		    }
		    
		    override def configFileName : Option[String] = None
		    override def configurationDefaults : ConfigurationDefaults = {
		      ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
		    }
	 
		    def representative( ptn : String ) : String = {
		      ptn
		    }
		    
		    def fits( ptn : String, place : String ) : Boolean = {
		      RegexPtn.matches( ptn, place ) || RegexPtn.matches( place, ptn )
		    }
		    
		    def fitsK(
		      ptn : String,
		      place : String
		    ) : Option[Substitution] = {
		      //println( "in fitsK on " + this )
		      if ( fits( ptn, place ) ) {
			Some( IdentitySubstitution() )
		      }
		      else {
			None
		      }
		    }
		    
		  }      
    }

    def handleEntry( grndTypeStr : String, imprtTypeStr : String, mTTTypeStr : String )( json : JValue, acc : Buffer[Elem] ) : Buffer[Elem] = {
      for(
	JObject( fvs ) <- json \\ "putval" ;
	JArray( valueArray ) <- json \\ "values" ;
	JArray( dstypes ) <- json \\ "dstypes" ;
	JArray( dsnames ) <- json \\ "dsnames" ;
	JDouble( time ) <- json \\ "time" ;
	JDouble( interval ) <- json \\ "interval" ; 
	JString( host ) <- json \\ "host" ; 
	JString( plugin ) <- json \\ "plugin" ;
	JString( plugin_instance ) <- json \\ "plugin_instance" ;
	JString( cdtype ) <- json \\ "type" ;
	JString( type_instance ) <- json \\ "type_instance"
      ) {
	val gvStr =
	  new XStream(
	    new JettisonMappedXmlDriver()
	  ).toXML( UUIDWrapper( ( getUUID + "" ) ) ).replace(
	    "com.biosimilarity.lift.lib.bulk.BulkCollectDImport$UUIDWrapper",
	    grndTypeStr
	  ).replace(
	    imprtTypeStr,
	    mTTTypeStr
	  ).replace(
	    "youyouid",
	    "v"
	  )
	
	acc +=
	<record>
	  <comProtegraAgentservicesstorePutVal>
	    <values>{for( JInt( v ) <- valueArray ) yield {<string>{v}</string>}}</values>
	    <dstypes>{for( JString( t ) <- dstypes ) yield {<string>{t}</string>}}</dstypes>
	    <dsnames>{for( JString( n ) <- dsnames ) yield {<string>{n}</string>}}</dsnames>
            <time>{<string>{time}</string>}</time>
	    <interval>{<string>{interval}</string>}</interval>
            <host>{<string>{host}</string>}</host>
            <plugin>{<string>{plugin}</string>}</plugin>
            <plugin_instance>{<string>{plugin_instance}</string>}</plugin_instance>
            <type>{<string>{cdtype}</string>}</type>
            <type_instance>{<string>{type_instance}</string>}</type_instance>
	  </comProtegraAgentservicesstorePutVal>
	  <string>{gvStr}</string>
	</record>
      }
      acc
    }

    def handleEntry( json : JValue, acc : Buffer[Elem] ) : Buffer[Elem] = {
      handleEntry(
	"com.biosimilarity.lift.model.store.MonadicTermTypes$Ground",
	"com.protegra.agentservicestore.usage.BulkCollectDImporter",
	"com.protegra.agentservicestore.usage.DieselEngineScope$TheMTT"
      )(
	json, acc
      )
    }
   
    @transient
    lazy val testRunID = getUUID
    @transient
    var runNum = 0

    @transient
    implicit lazy val pvOne =
      PutVal(
	List( 558815, 43649779 ),
	List( "derive", "derive" ),
	List( "rx", "tx" ),
	1334349094.633,
	10.000,
	"server-75530.localdomain",
	"interface",
	"eth0",
	"if_octets",
	""
      )
    @transient
       implicit lazy val pvTwo =
         PutVal(
   	List( 558815, 43649779 ),
   	List( "derive", "derive" ),
   	List( "rx", "tx" ),
   	1334349094.633,
   	10.000,
   	"jason.protegra",
   	"interface",
   	"eth0",
   	"if_octets",
   	""
         )
    @transient
    lazy val entryStr = 
      """ { "putval" : { "values":[558815,43649779],"dstypes":["derive","derive"],"dsnames":["rx","tx"],"time":1334349094.633,"interval":10.000,"host":"server-75530.localdomain","plugin":"interface","plugin_instance":"eth0","type":"if_octets","type_instance":"" } } """

    @transient
    implicit lazy val entryStream : Stream[String] =
      ( List( entryStr ) ).toStream append ( entryStream map { ( s : String ) => /* print( "." );*/ s } )    

    @transient
    lazy val rcrdStream : Stream[Elem] = {
      val acc = new ListBuffer[Elem]()
      val rcrd = handleEntry( parse( entryStr ), acc )( 0 )
      ( List( rcrd ) ).toStream append ( rcrdStream map { ( rcrd : Elem ) => /* print( "." ); */ rcrd } )
    }

    def readEntries(
      host : String, queue : String, file : String, lthrd : String, dbChunk : Int
    )( implicit eStrm : Stream[String] ) : String = {      
            
      val acc = rcrdStream.take( dbChunk )

      val rcrdsFileNameSfx = "" + getUUID + "-" + runNum + ""
      val recordsFileName = ( file + rcrdsFileNameSfx + ".xml" )
      val db = <records>{acc.toList}</records>

      println( "---------------------------******>>>>>>******---------------------------" )
      println( "\nsaving a chunk of records ( " + dbChunk + " ) to " + recordsFileName )
      println( "---------------------------******>>>>>>******---------------------------" )

      //scala.xml.XML.saveFull( recordsFileName, db, "UTF-8", true, null )

      recordsFileName
    }

    def importData( lthrd : String, chunkSize : Int )( implicit eStrm : Stream[String] ) : List[String] = {            
      List(
	readEntries( "localhost", "collectDSample", "collectDImport", lthrd, chunkSize )( eStrm )
      )
    }

    def loadData( numOfEntries : Int ) : Unit = {
      // Nothing to do
    }

    @transient
    val entryExchange : MonadicTupleSpace[String,String,String] = createExchange[String]()
    @transient
    val dataChunkExchange : MonadicTupleSpace[String,String,ListBuffer[Elem]] = createExchange[ListBuffer[Elem]]()

    def supplyEntries( host : String, queue : String, numOfEntries : Int ) : Unit = {
      // create an AMQP scope
      val collectDAMQPScope = new AMQPStdScope[String]()
      // create an AMQP Queue monad
      val collectDQM =
	new collectDAMQPScope.AMQPQueueHostExchangeM[String](
	  host,
	  queue
	)
      // get an empty queue
      val collectDQ = collectDQM.zero[String]    
      println( "---------------------------******>>>>>>******---------------------------" )
      println( "creating entries" )
      println( "---------------------------******>>>>>>******---------------------------" )

      val entry = """ { "putval" : { "values":[558815,43649779],"dstypes":["derive","derive"],"dsnames":["rx","tx"],"time":1334349094.633,"interval":10.000,"host":"server-75530.localdomain","plugin":"interface","plugin_instance":"eth0","type":"if_octets","type_instance":"" } } """

      lazy val entryStream : Stream[String] =
	( List( entry ) ).toStream append ( entryStream map { ( s : String ) => print( "." ); s } )

      for( i <- 1 to numOfEntries ) { collectDQ ! entryStream( i ) }

      println( "---------------------------******>>>>>>******---------------------------" )
      println( "\nentries created" )
      println( "---------------------------******>>>>>>******---------------------------" )
    }

    def readEntriesFromRabbitMQ(
      host : String, queue : String, file : String, lthrd : String, dbChunk : Int
    ) : ListBuffer[String] = {
      // create an AMQP scope
      val collectDAMQPScope = new AMQPStdScope[String]()
    // create an AMQP Queue monad
      val collectDQM =
	new collectDAMQPScope.AMQPQueueHostExchangeM[String](
	  host,
	  queue
	)
      // get an empty queue
      val collectDQ = collectDQM.zero[String]    
      
      val acc = new ListBuffer[Elem]()      
      val fileNames = new ListBuffer[String]()      
      val lock = new Lock()
      
      println( "reading entries" )
      for ( entry <- collectDQM( collectDQ ) ) {
	print( "." )
	handleEntry( parse( entry ), acc )
	lock.acquire
	if ( acc.size >= dbChunk ) {
	  runNum += 1
	  val rcrdsFileNameSfx = "" + testRunID + runNum + ""
	  val recordsFileName = ( file + rcrdsFileNameSfx + ".xml" )	    
	  val db = <records>{acc.toList}</records>
	  
	  println( "\nsaving a chunk of records ( " + dbChunk + " ) to " + recordsFileName )
	  //scala.xml.XML.saveFull( recordsFileName, db, "UTF-8", true, null )
	  fileNames += recordsFileName

	  println( "---------------------------******>>>>>>******---------------------------" )
	  println( "putting @ " + ( lthrd + "_" + recordsFileName ) )
	  println( "to trigger adding the data file to the kvdb node db" )
	  println( "---------------------------******>>>>>>******---------------------------" )

	  reset {
	    entryExchange.putS( lthrd + "_" + recordsFileName, rcrdsFileNameSfx )

	    println( "---------------------------******>>>>>>******---------------------------" )
	    println( "waiting @ " + ( lthrd + rcrdsFileNameSfx ) )
	    println( "to be able to continue processing json entries" )
	    println( "---------------------------******>>>>>>******---------------------------" )
	    
	    for( rsrc <- entryExchange.getS( lthrd + rcrdsFileNameSfx ) ) {

	      println( "---------------------------******>>>>>>******---------------------------" )
	      println( "got " + rsrc + " @ " + lthrd + rcrdsFileNameSfx )
	      println( "---------------------------******>>>>>>******---------------------------" )

	      rsrc match {
		case Some( msg ) => {
		  println( "found the droids we were looking for. " + msg )
		}
		case _ => {
		  throw new Exception( "unexpected communication: " + rsrc )
		}
	      }

	    }
	  }
	  acc.clear
	}
	lock.release	
      }      
      println( "\nentries read" )      
      fileNames
    }

    def loadDataToRabbitMQ( numOfEntries : Int ) : Unit = {
      supplyEntries( "localhost", "collectDSample", numOfEntries )
    }
    // def importDataFromRabbitMQ( lthrd : String, chunkSize : Int ) : List[String] = {            
//       val fileNameRoot = "collectDImport"
//       val filePtn = ( fileNameRoot + ".*" )
//       val lb = readEntriesFromRabbitMQ( "localhost", "collectDSample", "collectDImport", lthrd, chunkSize )
//       val s = new scala.collection.mutable.HashSet[String]( )

//       def loop( s : scala.collection.mutable.HashSet[String], n : Int ) : Unit = {
// 	println( "---------------------------******>>>>>>******---------------------------" )
// 	println( "waiting @ " + ( lthrd + "_" + filePtn ) )
// 	println( "to add a filename to the queue for adding to kvdb node db" )
// 	println( "---------------------------******>>>>>>******---------------------------" )

// 	reset {
// 	  for( rsrc <- entryExchange.getS( lthrd + "_" + filePtn ) ) {
// 	    println( "---------------------------******>>>>>>******---------------------------" )
// 	    println( "got " + rsrc + " @ " + lthrd + "_" + filePtn )
// 	    println( "---------------------------******>>>>>>******---------------------------" )
// 	    rsrc match {
// 	      case Some( fileNameSfx ) => {
// 		val fileName = fileNameRoot + fileNameSfx + ".xml"
		
// 		println( "---------------------------******>>>>>>******---------------------------" )
// 		println( "alerted that " + fileName + ".xml" + " has been written." )
// 		println( "---------------------------******>>>>>>******---------------------------" )
		
// 		s += fileName	      
		
// 		println( "---------------------------******>>>>>>******---------------------------" )
// 		println( "putting @ " + ( lthrd + fileNameSfx ) )
// 		println( "---------------------------******>>>>>>******---------------------------" )
		
// 		entryExchange.putS( lthrd + fileNameSfx, "move along." );
		
// 		if ( s.size < n ) { loop( s, n ) }
		
// 		() // to keep the type checker happy
// 	      }
// 	      case _ => {
// 		throw new Exception( "shouldn't be in this case" )
// 	      }
// 	    };
// 	    ()
// 	  }
// 	}
//       }

//       loop( s, 1 )

//       s.toList
//     }

  }

  object DieselConfigurationDefaults {
    val localHost : String = "localhost"
    val localPort : Int = 5672
    val remoteHost : String = "localhost"
    val remotePort : Int = 5672
    val dataLocation : String = "/cnxnTestProtocol"    
  }

  trait DieselManufactureConfiguration extends ConfigurationTrampoline {
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

  case class DieselManufacture( override val configFileName : Option[String] )
       extends DieselManufactureConfiguration {
    import DieselEngineScope._
    import Being._
    import AgentKVDBNodeFactory._

    import CnxnConversionStringScope._

    import com.protegra_ati.agentservices.store.extensions.StringExtensions._

    //override def configFileName : Option[String] = None
    override def configurationDefaults : ConfigurationDefaults = {
      DieselConfigurationDefaults.asInstanceOf[ConfigurationDefaults]
    }

    val cnxnGlobal = new acT.AgentCnxn("Global".toURI, "", "Global".toURI)

    def setup[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
      dataLocation : String,
      localHost : String, localPort : Int,
      remoteHost : String, remotePort : Int
    )(
      implicit returnTwist : Boolean
    ) : Either[Being.AgentKVDBNode[ReqBody,RspBody],(Being.AgentKVDBNode[ReqBody, RspBody],Being.AgentKVDBNode[ReqBody, RspBody])] = {
      val ( localExchange, remoteExchange ) = 
	if ( localHost.equals( remoteHost ) && ( localPort == remotePort ) ) {
	  ( dataLocation, dataLocation + "Remote" )	  
	}
	else {
	  ( dataLocation, dataLocation )	  
	}

      if ( returnTwist ) {
	Right[Being.AgentKVDBNode[ReqBody,RspBody],(Being.AgentKVDBNode[ReqBody, RspBody],Being.AgentKVDBNode[ReqBody, RspBody])](
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
	Left[Being.AgentKVDBNode[ReqBody, RspBody],(Being.AgentKVDBNode[ReqBody, RspBody],Being.AgentKVDBNode[ReqBody, RspBody])](
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
    ) : Either[Being.AgentKVDBNode[ReqBody,RspBody],(Being.AgentKVDBNode[ReqBody, RspBody],Being.AgentKVDBNode[ReqBody, RspBody])] = {
      setup( "/agentUseCaseProtocol", localHost, localPort, remoteHost, remotePort )
    }

    def agent[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( 
      dataLocation : String
    ) : Being.AgentKVDBNode[ReqBody,RspBody] = {
      val Right( ( client, server ) ) = 
	setup[ReqBody,RspBody](
	  dataLocation, "localhost", 5672, "localhost", 5672
	)( true )
      client
    }	 

    def fileNameToCnxn( fileName : String ) : acT.AgentCnxn = {
      val fileNameRoot = fileName.split( '/' ).last
      new acT.AgentCnxn( fileNameRoot.toURI, "", fileNameRoot.toURI )
    } 
  }  
}
