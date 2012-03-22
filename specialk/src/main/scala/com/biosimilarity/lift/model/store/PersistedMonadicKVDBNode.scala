// -*- mode: Scala;-*- 
// Filename:    PersistedMonadicKVDBNode.scala 
// Authors:     lgm                                                    
// Creation:    Thu Mar 22 09:24:10 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._
import net.liftweb.amqp._

import scala.util.continuations._ 
import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
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
import java.net.URI
import java.io.File
import java.io.FileInputStream
import java.io.OutputStreamWriter
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayOutputStream


trait PersistedMonadicKVDBNodeScope[Namespace,Var,Tag,Value] 
extends MonadicKVDBNodeScope[Namespace,Var,Tag,Value] with Serializable {  
  trait PersistenceScope
    extends ExcludedMiddleScope[mTT.GetRequest,mTT.GetRequest,mTT.Resource]
    with Serializable {
      trait PersistenceManifest extends Serializable {
	def db : Database
	def storeUnitStr[Src,Label,Trgt]( cnxn : Cnxn[Src,Label,Trgt] ) : String
	def storeUnitStr : String
	def toFile( ptn : mTT.GetRequest ) : Option[File]
	def query( ptn : mTT.GetRequest ) : Option[String]
	def query( xmlCollStr : String, ptn : mTT.GetRequest ) : Option[String]
	def kquery( xmlCollStr : String, ptn : mTT.GetRequest ) : Option[String]
	
	def valueStorageType : String
	def continuationStorageType : String
	
	def labelToNS : Option[String => Namespace]
	def textToVar : Option[String => Var]
	def textToTag : Option[String => Tag]        
	
	def kvNameSpace : Namespace
	def kvKNameSpace : Namespace
	
	def compareNameSpace( ns1 : Namespace, ns2 : Namespace ) : Boolean
	
	def asStoreKey(
	  key : mTT.GetRequest
	) : CnxnCtxtLabel[Namespace,Var,String] with Factual
	
	def asStoreValue(
	  rsrc : mTT.Resource
	) : CnxnCtxtLeaf[Namespace,Var,String] with Factual
	
	def asStoreRecord(
	  key : mTT.GetRequest,
	  value : mTT.Resource
	) : CnxnCtxtLabel[Namespace,Var,String] with Factual
	
	def asStoreKRecord(
	  key : mTT.GetRequest,
	  value : mTT.Resource
	) : CnxnCtxtLabel[Namespace,Var,String] with Factual
	
	def asCacheValue(
	  ccl : CnxnCtxtLabel[Namespace,Var,String]
	) : Value    
	
	def asCacheValue(
	  ltns : String => Namespace,
	  ttv : String => Var,
	  value : Elem
	) : Option[Value]
	
	def asCacheK(
	  ccl : CnxnCtxtLabel[Namespace,Var,String]
	) : mTT.Continuation = {
	  throw new Exception( "shouldn't be calling this version of asCacheK" )
	}
	
	def asCacheK(
	  ltns : String => Namespace,
	  ttv : String => Var,
	  value : Elem
	) : Option[mTT.Continuation] = {
	  throw new Exception( "shouldn't be calling this version of asCacheK" )
	}

	def asResource(
	  key : mTT.GetRequest, // must have the pattern to determine bindings
	  value : Elem
	) : emT.PlaceInstance 
	
	def recordDeletionQueryTemplate : String = {
	  "delete node let $key := %RecordKeyConstraints% for $rcrd in collection( '%COLLNAME%' )//record where deep-equal($rcrd/*[1], $key) return $rcrd"
	}
      }
      
      trait PersistenceManifestTrampoline extends Serializable {
	def persistenceManifest : Option[PersistenceManifest] = None
	
	def storeUnitStr[Src,Label,Trgt](
	  cnxn : Cnxn[Src,Label,Trgt]
	) : Option[String] = {
	  for( pd <- persistenceManifest ) 
	  yield {
	    pd.storeUnitStr( cnxn )
	  }
	}
	
	def storeUnitStr : Option[String] = {
	  for( pd <- persistenceManifest ) 
	  yield {
	    pd.storeUnitStr
	  }
	}
	
	def query( ptn : mTT.GetRequest ) : Option[String] = {
	  for( pd <- persistenceManifest; qry <- pd.query( ptn ) ) 
	  yield {
	    qry
	  }
	}
	
	def query( xmlCollStr : String, ptn : mTT.GetRequest ) : Option[String] = {
	  for( pd <- persistenceManifest; qry <- pd.query( xmlCollStr, ptn ) ) 
	  yield {
	    qry
	  }
	}
	
	def kquery( xmlCollStr : String, ptn : mTT.GetRequest ) : Option[String] = {
	  for( pd <- persistenceManifest; qry <- pd.kquery( xmlCollStr, ptn ) ) 
	  yield {
	    qry
	  }
	}
	
	def labelToNS : Option[String => Namespace] = {
	  for( pd <- persistenceManifest; ltns <- pd.labelToNS ) 
	  yield {
	    ltns
	  }
	}
	def textToVar : Option[String => Var] = {
	  for( pd <- persistenceManifest; ttv <- pd.textToVar ) 
	  yield {
	    ttv
	  }
	}
	def textToTag : Option[String => Tag] = {
	  for( pd <- persistenceManifest; ttt <- pd.textToTag ) 
	  yield {
	    ttt
	  }
	}
	
	def kvNameSpace : Option[Namespace] = {
	  for( pd <- persistenceManifest )
	  yield { pd.kvNameSpace }
	}
	
	def kvKNameSpace : Option[Namespace] = {
	  for( pd <- persistenceManifest )
	  yield { pd.kvKNameSpace }
	}
	
	def compareNameSpace( ns1 : Namespace, ns2 : Namespace ) : Boolean = {
	  persistenceManifest match {
	    case Some( pd ) => pd.compareNameSpace( ns1, ns2 )
	    case _ => false
	  }
	}
	
	def asStoreKey(
	  key : mTT.GetRequest
	) : Option[CnxnCtxtLabel[Namespace,Var,String] with Factual] = {
	  for( pd <- persistenceManifest )
	  yield { pd.asStoreKey( key ) }
	}
	
	def asStoreValue(
	  rsrc : mTT.Resource
	) : Option[CnxnCtxtLeaf[Namespace,Var,String] with Factual] = {
	  for( pd <- persistenceManifest ) 
	  yield { pd.asStoreValue( rsrc ) }
	}
	
	def asStoreRecord(
	  key : mTT.GetRequest,
	  value : mTT.Resource
	) : Option[CnxnCtxtLabel[Namespace,Var,String] with Factual] = {
	  for( pd <- persistenceManifest )
	  yield { pd.asStoreRecord( key, value ) }
	}
	
	def asStoreKRecord(
	  key : mTT.GetRequest,
	  value : mTT.Resource
	) : Option[CnxnCtxtLabel[Namespace,Var,String] with Factual] = {
	  for( pd <- persistenceManifest )
	  yield { pd.asStoreKRecord( key, value ) }
	}
	
	def asResource(
	  key : mTT.GetRequest, // must have the pattern to determine bindings
	  value : Elem
	) : Option[emT.PlaceInstance] = {
	  for( pd <- persistenceManifest )
	  yield { pd.asResource( key, value ) }
	}
	
	def asCacheValue(
	  ccl : CnxnCtxtLabel[Namespace,Var,String]
	) : Option[Value] = {
	  for( pd <- persistenceManifest )
	  yield { pd.asCacheValue( ccl ) }
	}
	
	def asCacheValue(
	  ltns : String => Namespace,
	  ttv : String => Var,
	  value : Elem
	) : Option[Value] = {
	  for(
	    pd <- persistenceManifest;
	    rsrc <- pd.asCacheValue( ltns, ttv, value )
	  )	yield { rsrc }
	}
	
	def asCacheK(
	  ccl : CnxnCtxtLabel[Namespace,Var,String]
	) : Option[mTT.Continuation] = {
	  for( pd <- persistenceManifest )
	  yield { pd.asCacheK( ccl ) }
	}
	
	def asCacheK(
	  ltns : String => Namespace,
	  ttv : String => Var,
	  value : Elem
	) : Option[mTT.Continuation] = {
	  for(
	    pd <- persistenceManifest;
	    rsrc <- pd.asCacheK( ltns, ttv, value )
	  )	yield { rsrc }
	}
      }      

      abstract class XMLDBManifest(
	@transient override val db : Database
      ) extends PersistenceManifest 
	       with PrologMgr
	       with CnxnConversions[Namespace,Var,Tag]
	       with CnxnXQuery[Namespace,Var,Tag]
	       with CnxnXML[Namespace,Var,Tag]
	       with CnxnCtxtInjector[Namespace,Var,Tag]
	       with XMLIfy[Namespace,Var]
	       with Blobify
	       with UUIDOps
      {
	import CnxnConversionStringScope._
	// BUGBUG -- LGM: Why not just the identity?
	override def asStoreKey(
	  key : mTT.GetRequest
	) : CnxnCtxtLabel[Namespace,Var,String] with Factual = {
	  key match {
	    case CnxnCtxtLeaf( Left( t ) ) =>
	      new CnxnCtxtLeaf[Namespace,Var,String](
		Left( t + "" )	    
	      )
	    case CnxnCtxtLeaf( Right( v ) ) =>
	      new CnxnCtxtLeaf[Namespace,Var,String](
		Right( v )
	      )
	    case CnxnCtxtBranch( ns, facts ) =>
	      new CnxnCtxtBranch[Namespace,Var,String](
		ns,
		facts.map( asStoreKey )
	      )
	  }
	}
	
	def asStoreEntry(
	  key : mTT.GetRequest,
	  value : mTT.Resource
	)(
	  nameSpace : Namespace
	): CnxnCtxtLabel[Namespace,Var,String] with Factual = {
	  new CnxnCtxtBranch[Namespace,Var,String](
	    nameSpace,
	    List( asStoreKey( key ), asStoreValue( value ) )
	  )
	}

	override def asStoreRecord(
	  key : mTT.GetRequest,
	  value : mTT.Resource
	) : CnxnCtxtLabel[Namespace,Var,String] with Factual = {
	  asStoreEntry( key, value )( kvNameSpace )
	}

	override def asStoreKRecord(
	  key : mTT.GetRequest,
	  value : mTT.Resource
	) : CnxnCtxtLabel[Namespace,Var,String] with Factual = {
	  println( "in asStoreKRecord with kvKNameSpace = " + kvKNameSpace )
	  asStoreEntry( key, value )( kvKNameSpace )
	}

	override def asCacheValue(
	  ccl : CnxnCtxtLabel[Namespace,Var,String]
	) : Value    
	
	override def asCacheValue(
	  ltns : String => Namespace,
	  ttv : String => Var,
	  value : Elem
	) : Option[Value] = {
	  val ttt = ( x : String ) => x
	  xmlIfier.fromXML( ltns, ttv, ttt )( value ) match {
	    case Some( CnxnCtxtBranch( ns, k :: v :: Nil ) ) => {
	      val vale : Value =
		asCacheValue(	      
		  v.asInstanceOf[CnxnCtxtLabel[Namespace,Var,String]]
		)
	      if ( kvNameSpace.equals( ns ) ) {	    
		Some( vale )
	      }
	      else {	    
		None
	      }
	    }
	    case v@_ => {
	      None
	    }
	  }
	}    
	
	override def asCacheK(
	  ccl : CnxnCtxtLabel[Namespace,Var,String]
	) : mTT.Continuation = {
	  ccl match {
	    case CnxnCtxtBranch(
	      "string",
	      CnxnCtxtLeaf( Left( rv ) ) :: Nil
	    ) => {
	      val unBlob =
		continuationStorageType match {
		  case "CnxnCtxtLabel" => {
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
		  k.asInstanceOf[mTT.Continuation]
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
	  ltns : String => Namespace,
	  ttv : String => Var,
	  value : Elem
	) : Option[mTT.Continuation] = {
	  throw new Exception( "shouldn't be calling this version of asCacheK" )
	}		
	
	override def query(
	  ptn : mTT.GetRequest
	) : Option[String] = {
	  for( ttv <- textToVar )
	  yield {
	    xqQuery(
	      new CnxnCtxtBranch[Namespace,Var,Tag](
		kvNameSpace,
		List(
		  asCCL( ptn ),
		  new CnxnCtxtLeaf[Namespace,Var,Tag](
		    Right(
		      ttv( "VisForValueVariableUniqueness" )
		    )
		  )
		)
	      )
	    )
	  }
	}
	
	def queryRsrc(
	  xmlCollStr : String,
	  ptn : mTT.GetRequest
	)(
	  nameSpace : Namespace
	): Option[String] = {
	  for( ttv <- textToVar )
	  yield {
	    val ccb =
	      new CnxnCtxtBranch[Namespace,Var,Tag](
		nameSpace,
		List(
		  asCCL( ptn ),
		  new CnxnCtxtLeaf[Namespace,Var,Tag](
		    Right(
		      ttv( "VisForValueVariableUniqueness" )
		    )
		  )
		)
	      )
	    xqQuery(
	      ccb,
	      XQCC(
		Some( ccb ),
		ccb,
		"collection( '%COLLNAME%' )/".replace(
		  "%COLLNAME%",
		  xmlCollStr
		),
		Some( nextXQV ),
		None, None,
		DeBruijnIndex( 0, 0 )
	      )
	    )
	  }
	}
	
	override def query(
	  xmlCollStr : String,
	  ptn : mTT.GetRequest
	) : Option[String] = {
	  queryRsrc( xmlCollStr, ptn )( kvNameSpace )
	}
	
	override def kquery(
	  xmlCollStr : String,
	  ptn : mTT.GetRequest
	) : Option[String] = {
	  queryRsrc( xmlCollStr, ptn )( kvKNameSpace )
	}
	
	override def toFile(
	  ptn : mTT.GetRequest
	) : Option[File] = {
	  // TBD
	  None
	}    	
      }

      object XMLDBManifest {
	def unapply(
	  ed : XMLDBManifest
	) : Option[( Database )] = {
	  Some( ( ed.db ) )
	}
      }

      abstract class AbstractPersistedMonadicKVDB[ReqBody, RspBody](
	override val name : Moniker
      ) extends Individual[ReqBody,RspBody,AbstractPersistedMonadicKVDBNode](
	name,
	new ListBuffer[JustifiedRequest[ReqBody,RspBody]](),
	new ListBuffer[JustifiedResponse[ReqBody,RspBody]]()
      ) with MonadicTermStoreT
	       with PersistenceManifestTrampoline
	       with BaseXXMLStore           
	       with BaseXCnxnStorage[Namespace,Var,Tag]
      {    
      }
  
      abstract class AbstractPersistedMonadicKVDBNode[ReqBody, RspBody](
	val localCache : AbstractPersistedMonadicKVDB[ReqBody,RspBody],
	override val acquaintances : List[Moniker]
      ) extends MonadicTxPortFramedMsgDispatcher[String,ReqBody,RspBody,AbstractPersistedMonadicKVDBNode](
	localCache, acquaintances
      ) with MonadicTermStoreT {
	import identityConversions._
	
	override def txPort2FramedMsg [A <: FramedMsg] ( txPortMsg : String ) : A = {
	  tweet( "unwrapping transport message : " + txPortMsg )
	  // BUGBUG -- lgm : there's a bug in the JettisonMappedXmlDriver
	  // that misses the option declaration inside the RBound subtype
	  // of Resource; so, the workaround is to use XML instead of JSON
	  //val xstrm = new XStream( new JettisonMappedXmlDriver )
	  val xstrm = new XStream( )
	  val fmsg = xstrm.fromXML( txPortMsg )
	  tweet( "resulting framed message : " + fmsg )
	  fmsg.asInstanceOf[A]
	}
	override def framedMsg2TxPort [A >: FramedMsg] ( txPortMsg : A ) : String = {
	  tweet( "wrapping framed message : " + txPortMsg )
	  //val xstrm = new XStream( new JettisonMappedXmlDriver )
	  val xstrm = new XStream( )
	  val xmsg = xstrm.toXML( txPortMsg )
	  tweet( "resulting transport message : " + xmsg )
	  xmsg
	}
      }
      
      case class PersistedMonadicKVDB(
	override val name : Moniker
      ) extends AbstractPersistedMonadicKVDB[KVDBNodeRequest,KVDBNodeResponse](
	name
      ) {
	override def configFileName : Option[String] = None
	override def configurationDefaults : ConfigurationDefaults = {
	  ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
	} 

	override def tmpDirStr : String = {
	  val tds = config.getString( "storageDir", "tmp" )       
	  val tmpDir = new java.io.File( tds )
	  if ( ! tmpDir.exists ) {
	    tmpDir.mkdir
	  }
	  tds
	} 
	
	def asCursor(
	  values : List[mTT.Resource]
	) : Option[mTT.Resource] = {
	  
	  val ig : mTT.Generator[mTT.Resource, Unit, Unit]  = mTT.itergen[mTT.Resource]( values )
	  
	  // BUGBUG -- LGM need to return the Solution
	  // Currently the PersistenceManifest has no access to the
	  // unification machinery
	  Some (
            mTT.RBoundHM(
	      Some( mTT.Cursor( ig ) ),
	      None
  	    )
          )
	}
	
	override def asCacheValue(
	  ltns : String => Namespace,
	  ttv : String => Var,
	  value : Elem
	) : Option[Value] = {      
	  tweet(
	    "converting store value to cache value"
	  )
	  valueStorageType match {
	    case "CnxnCtxtLabel" => {
	      tweet(
		"using CnxnCtxtLabel method"
	      )
	      val ttt = ( x : String ) => x
	      xmlIfier.fromXML( ltns, ttv, ttt )( value ) match {
		case Some( CnxnCtxtBranch( ns, k :: v :: Nil ) ) => {
		  tweet(
		    "Good news! Value has the shape of a record"
		  )
		  if ( kvNameSpace.getOrElse( "" ).equals( ns ) ) {
		    tweet(
		      "namespace matches : " + ns
		    )
		    tweet(
		      "value before conversion is \n" + v
		    )
		    for(
		      vale <-
		      asCacheValue(	      
			v.asInstanceOf[CnxnCtxtLabel[Namespace,Var,String]]
		      )		
		    ) yield { vale }
		  }
		  else {
		    if ( kvKNameSpace.getOrElse( "" ).equals( ns ) ) {
		      tweet(
			"namespace matches : " + ns
		      )
		      tweet(
			"value before conversion is \n" + v
		      )
		      for(
			vale <-
			asCacheValue(	      
			  v.asInstanceOf[CnxnCtxtLabel[Namespace,Var,String]]
			)		
		      ) yield { vale }
		    } else {
		      tweet(
			(
			  "namespace mismatch: "
			  + "kvNameSpace : " + kvNameSpace
			  + "kvKNameSpace : " + kvKNameSpace
			  + "," + ns
			)
		      )
		      None
		    }
		  }
		}
		case _ => {
		  tweet(
		    "Value failed to embody the shape of a record" + value
		  )
		  None
		}
	      }
	    }
	    case "XStream" => {
	      tweet(
		"using XStream method"
	      )
	      xmlIfier.fromXML( ltns, ttv )( value ) match {
		case Some( CnxnCtxtBranch( ns, k :: v :: Nil ) ) => {
		  v match {
		    case CnxnCtxtLeaf( Left( t ) ) => {
		      Some(
			new XStream(
			  new JettisonMappedXmlDriver
			).fromXML( t ).asInstanceOf[Value]
		      )
		    }
		    case _ => None
		  }	      
		}
		case v@_ => {
		  None
		}
	      }	  
	    }
	    case _ => {
	      throw new Exception( "unexpected value storage type" )
	    }
	  }      
	}    
	
	def putInStore(
	  persist : Option[PersistenceManifest],
	  channels : Map[mTT.GetRequest,mTT.Resource],
	  ptn : mTT.GetRequest,
	  wtr : Option[mTT.GetRequest],
	  rsrc : mTT.Resource,
	  collName : Option[String]
	) : Unit = {
	  persist match {
	    case None => {
	      channels( wtr.getOrElse( ptn ) ) = rsrc	  
	    }
	    case Some( pd ) => {
	      tweet( "accessing db : " + pd.db )
	      // remove this line to force to db on get
	      channels( wtr.getOrElse( ptn ) ) = rsrc	  
	      spawn {
		for(
		  rcrd <- asStoreRecord( ptn, rsrc );
		  sus <- collName
		) {
		  tweet(
		    (
		      "storing to db : " + pd.db
		      + " pair : " + rcrd
		      + " in coll : " + sus
		    )
		  )
		  store( sus )( rcrd )
		}
	      }
	    }
	  }
	}
	
	// BUGBUG -- lgm : how can we refactor out commonality between
	// this and the method, putInStore, above
	def putKInStore(
	  persist : Option[PersistenceManifest],
	  ptn : mTT.GetRequest,
	  rsrc : mTT.Resource,
	  collName : Option[String]
	) : Unit = {
	  persist match {
	    case None => {
	      // Nothing to do
	      tweet( "warning : no store in which to put continuation " + rsrc )
	    }
	    case Some( pd ) => {
	      tweet( "putKInStore accessing db : " + pd.db )
	      spawn {
		for(
		  rcrd <- asStoreKRecord( ptn, rsrc );
		  sus <- collName
		) {
		  tweet(
		    (
		      "storing to db : " + pd.db
		      + " pair : " + rcrd
		      + " in coll : " + sus
		    )
		  )
		  store( sus )( rcrd )
		}
	      }
	    }
	  }
	}
	
	def updateKStore( persist : Option[PersistenceManifest] )( 
	  ptn : mTT.GetRequest,
	  consume : Boolean,
	  collName : Option[String]
	) : Option[List[emT.PlaceInstance]] = {
	  tweet( "in updateKStore " )
	  val xmlCollName =
	    collName.getOrElse(
	      storeUnitStr.getOrElse(
		bail()
	      )
	    )
	  
	  checkIfDBExistsAndCreateIfNot( xmlCollName, true ) match {
	    case true => {
	      tweet( "database " + xmlCollName + " found" )
	      val oKQry = kquery( xmlCollName, ptn )
	      oKQry match {
		case None => {
		  throw new Exception(
		    "failed to compile a continuation query" 
		  )				  
		}
		case Some( kqry ) => {
		  tweet( "kqry : " + kqry )
		  val krslts = executeWithResults( kqry )
		  krslts match {
		    case Nil => {
		      // Nothing to do
		      tweet( " no continuations in store " )
		      None
		    }
		    case _ => {
		      for( pm <- persist ) yield {
			for( krslt <- krslts ) yield {
			  tweet( "retrieved " + krslt.toString )
			  val ekrsrc = pm.asResource( ptn, krslt )
			  tweet( "krslt as resource " + ekrsrc )
			  ekrsrc.stuff match {
			    case Right( k :: ks ) => {
			      tweet( "have a list of continuations " )
			      if ( consume ) {
				// BUGBUG -- lgm : write XQuery to update node
				tweet( "removing from store " + krslt )
				removeFromStore( 
				  persist,
				  krslt,
				  collName
				)
				tweet( "updating store " )
				putKInStore(
				  persist,
				  ptn,
				  mTT.Continuation( ks ),
				  collName
				)
			      }
			      ekrsrc
			    }
			    case Right( Nil ) => {
			      tweet( " have empty list of continuations; no continuations in store " )
			      ekrsrc
			    }
			    case _ => {
			      throw new Exception(
				"Non-continuation resource stored in kRecord" + ekrsrc
			      )
			    }
			  }
			}
		      }
		    }
		  }
		}
	      }
	    }
	    case false => {
	      tweet( "warning: failed to find a database!" )			  
	      None
	    }
	  }
	}
	
	def putPlaces( persist : Option[PersistenceManifest] )(
	  channels : Map[mTT.GetRequest,mTT.Resource],
	  registered : Map[mTT.GetRequest,List[RK]],
	  ptn : mTT.GetRequest,
	  rsrc : mTT.Resource,
	  consume : Boolean,
	  collName : Option[String]
	) : Generator[emT.PlaceInstance,Unit,Unit] = {    
	  Generator {
	    k : ( emT.PlaceInstance => Unit @suspendable ) => 
	      // Are there outstanding waiters at this pattern?    
	      val map =
		Right[
		  Map[mTT.GetRequest,mTT.Resource],
		  Map[mTT.GetRequest,List[RK]]
		]( registered )
	    val waitlist = locations( map, ptn )
	    
	    waitlist match {
	      // Yes!
	      case waiter :: waiters => {
		tweet( "found waiters waiting for a value at " + ptn )
		val itr = waitlist.toList.iterator	    
		while( itr.hasNext ) {
		  // BUGBUG -- lgm : SHOULD NOT HAVE TO CAST
		  k( itr.next.asInstanceOf[emT.PlaceInstance] )
		}
	      }
	      // No...
	      case Nil => {
		// Store the rsrc at a representative of the ptn
		tweet( "no waiters waiting for a value at " + ptn )
		//channels( representative( ptn ) ) = rsrc
		updateKStore( persist )(
		  ptn, consume, collName
		) match {
		  case Some( pIs ) => {
		    for ( pI <- pIs ) {
		      pI.stuff match {
			case Right( k :: ks ) => {
			  for( sk <- ( k :: ks ) ) {
			    spawn {
			      sk( pI.subst( rsrc ) )
			    }
			  }
			}
			case Right( Nil ) => {
			  putInStore(
			    persist, channels, ptn, None, rsrc, collName
			  )
			}
		      }
		    }
		  }
		  case None => {
		    putInStore(
		      persist, channels, ptn, None, rsrc, collName
		    )
		  }
		}		
	      }
	    }
	  }
	}
	
	def removeFromStore(
	  persist : Option[PersistenceManifest],
	  record : Elem,
	  collName : Option[String]
	) : Unit = {
	  for( pd <- persist; clNm <- collName )
	    {
	      tweet( "removing from db : " + pd.db )
	      val rcrdKey =
		record match {
		  case <record>{ kv @_* }</record> => {
		    val kvs = kv.toList.filter(
		      ( n : Node ) => {
			n match {
			  case Text( contents ) => {
			    !java.util.regex.Pattern.matches( 
			      "(\\p{Space}|\\p{Blank})*",
			      contents
			    )
			  }
			  case e : Elem => {
			    true
			  }
			}
		      }
		    )
		    kvs match {
		      case k :: v :: Nil => k.toString
		      case _ => 
			throw new Exception(
			  "Not a k-v record shape\n" + kvs
			)
		    }
		  }
		  case <kRecord>{ kv @_* }</kRecord> => {
		    val kvs = kv.toList.filter(
		      ( n : Node ) => {
			n match {
			  case Text( contents ) => {
			    !java.util.regex.Pattern.matches( 
			      "(\\p{Space}|\\p{Blank})*",
			      contents
			    )
			  }
			  case e : Elem => {
			    true
			  }
			}
		      }
		    )
		    kvs match {
		      case k :: v :: Nil => k.toString
		      case _ => 
			throw new Exception(
			  "Not a k-v record shape\n" + kvs
			)
		    }
		  }
		  case _ =>
		    throw new Exception(
		      "Not a record\n" + record
		    )
		}
	      
	      val deletionQry =
		pd.recordDeletionQueryTemplate.replace(
		  "%RecordKeyConstraints%",
		  rcrdKey
		).replace(
		  "%COLLNAME%",
		  clNm
		)
	      tweet( "deletion query : \n" + deletionQry )
	      val ostrm = new java.io.ByteArrayOutputStream()
	      execute( List( deletionQry ) )
	      tweet(
		"deletion results: \n" + ostrm.toString
	      )
	    }
	}

	def mput( persist : Option[PersistenceManifest] )(
	  channels : Map[mTT.GetRequest,mTT.Resource],
	  registered : Map[mTT.GetRequest,List[RK]],
	  consume : Boolean,
	  collName : Option[String]
	)(
	  ptn : mTT.GetRequest,
	  rsrc : mTT.Resource
	) : Unit @suspendable = {                            
	  for(
	    placeNRKsNSubst
	    <- putPlaces(
	      persist
	    )( channels, registered, ptn, rsrc, consume, collName )
	  ) {	      
	    val emT.PlaceInstance( wtr, Right( rks ), s ) = placeNRKsNSubst
	    tweet( "waiters waiting for a value at " + wtr + " : " + rks )
	    updateKStore( persist )(
	      ptn, consume, collName
	    ) match {
	      case Some( pIs ) => {
		for ( pI <- pIs ) {
		  pI.stuff match {
		    case Right( k :: ks ) => {		      
		      for( sk <- ( k :: ks ) ) {			
			spawn {
			  sk( pI.subst( rsrc ) )
			}
		      }
		    }
		    case Right( Nil ) => {
		      putInStore(
			persist, channels, ptn, None, rsrc, collName
		      )
		    }
		  }
		}
	      }
	      case None => {
		putInStore(
		  persist, channels, ptn, None, rsrc, collName
		)
	      }
	    }            
	  }
	}
	
	def mget(
	  persist : Option[PersistenceManifest],
	  ask : dAT.Ask,
	  hops : List[Moniker]
	)(
	  channels : Map[mTT.GetRequest,mTT.Resource],
	  registered : Map[mTT.GetRequest,List[RK]],
	  consume : Boolean,
	  cursor : Boolean,
	  collName : Option[String]
	)(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {        
	  Generator {	
	    rk : ( Option[mTT.Resource] => Unit @suspendable ) =>
	      shift {
		outerk : ( Unit => Unit ) =>
		  reset {
		    for(
		      oV <- mget( channels, registered, consume )( path ) 
		    ) {
		      oV match {
			case None => {
			  persist match {
			    case None => {
			      // tweet( ">>>>> no persistence manifest..." )
// 			      tweet( ">>>>> forwarding..." )
// 			      forward( ask, hops, path )
			      rk( oV )
			    }
			    case Some( pd ) => {
			      tweet( ">>>>> found a persistence manifest..." )
			      tweet(
				"accessing db : " + pd.db
			      )
			      
			      val xmlCollName =
				collName.getOrElse(
				  storeUnitStr.getOrElse(
				    bail()
				  )
				)
			      
			      // Defensively check that db is actually available
			      
			      checkIfDBExistsAndCreateIfNot( xmlCollName, true ) match {
				case true => {
				  val oQry = query( xmlCollName, path )
				  
				  oQry match {
				    case None => {
				      // tweet( ">>>>> unable to compile query for path " + path )
// 				      tweet( ">>>>> forwarding..." )
// 				      forward( ask, hops, path )
				      rk( oV )
				    }
				    case Some( qry ) => {	
				      tweet( ">>>>> compiled query for path " + path )
				      tweet(
					(
					  "retrieval query : \n" + qry
					)
				      )
				      
				      val rslts = executeWithResults( qry )
				      
				      rslts match {
					case Nil => {	
					  tweet(
					    (
					      "database "
					      + xmlCollName
					      + " had no matching resources."
					    )
					  )
					  
					  // Need to store the
					  // continuation on the tail of
					  // the continuation entry
					  val oKQry = kquery( xmlCollName, path )
					  oKQry match {
					    case None => {
					      throw new Exception(
						"failed to compile a continuation query" 
					      )				  
					    }
					    case Some( kqry ) => {
					      val krslts = executeWithResults( qry )
					      
					      // This is the easy case!
					      // There are no locations
					      // matching the pattern with
					      // stored continuations	  					  
					      krslts match {
						case Nil => {
						  putKInStore(
						    persist,
						    path,
						    mTT.Continuation( List( rk ) ),
						    collName
						  )
						}
						case _ => {
						  // A more subtle
						  // case. Do we store
						  // the continutation on
						  // each match?
						  // Answer: Yes!
						  for( krslt <- itergen[Elem]( krslts ) ) {
						    tweet( "retrieved " + krslt.toString )
						    val ekrsrc = pd.asResource( path, krslt )
						    
						    ekrsrc.stuff match {
						      case Right( ks ) => {  
							tweet( "removing from store " + krslt )
							removeFromStore( 
							  persist,
							  krslt,
							  collName
							)
							putKInStore(
							  persist,
							  path,
							  mTT.Continuation( ks ++ List( rk ) ),
							  collName
							)
						      }
						      case _ => {
							throw new Exception(
							  "Non-continuation resource stored in kRecord" + ekrsrc
							)
						      }
						    }
						  }
						}
					      }				  
					    }
					  }	
					  
					  // Then forward the request
					  //forward( ask, hops, path )
					  rk( oV )
					}
					case _ => { 			  
					  tweet(
					    (
					      "database "
					      + xmlCollName
					      + " had "
					      + rslts.length
					      + " matching resources."
					    )
					  )		  				  
					  
					  if ( cursor )
					    {
                                              var rsrcRslts : List[mTT.Resource] = Nil
                                              for( rslt <- itergen[Elem]( rslts ) ) {
						tweet( "retrieved " + rslt.toString )
						
						if ( consume ) {
						  tweet( "removing from store " + rslt )
						  removeFromStore(
						    persist,
						    rslt,
						    collName
						  )
						}
						
						// BUGBUG -- LGM : This is a
						// window of possible
						// failure; if we crash here,
						// then the result is out of
						// the store, but we haven't
						// completed processing. This is
						// where we need Tx.

						val ersrc : emT.PlaceInstance = pd.asResource( path, rslt )
						ersrc.stuff match {
						  case Left( r ) => rsrcRslts = r :: rsrcRslts
						  case _ => {}
						}
						
                                              }
					      
                                              val rsrcCursor = asCursor( rsrcRslts )
                                              //tweet( "returning cursor" + rsrcCursor )
                                              rk( rsrcCursor )
					    }
						else
						  {
						    for( rslt <- itergen[Elem]( rslts ) ) {
						      tweet( "retrieved " + rslt.toString )
						      val ersrc = pd.asResource( path, rslt )
						      
						      if ( consume ) {
							tweet( "removing from store " + rslt )
							removeFromStore( 
							  persist,
							  rslt,
							  collName
							)
						      }
						      
						      // BUGBUG -- LGM : This is a
						      // window of possible
						      // failure; if we crash here,
						      // then the result is out of
						      // the store, but we haven't
						      // completed processing. This is
						      // where we need Tx.
						      
						      ersrc.stuff match {
							case Left( r ) => {
							  tweet( "returning " + r )
							  rk( Some( r ) )
							}
							case _ => {
							  throw new Exception(
							    "violated excluded middle contract: " + ersrc
							  )
							}
						      }					  
						    }
						  }
					}
				      }
				    }			    
				  }			      
				}
				case false => {
				  // tweet( ">>>>> forwarding..." )
// 				  forward( ask, hops, path )
				  rk( oV )
				}
			      }
			    }		      
			  }
			}
			case _ => rk( oV )
		      }
		    }
		  }
	      }
	  }
	}
      }
      case class PersistedMonadicKVDBNode(
	cache : PersistedMonadicKVDB,
	override val acquaintances : List[Moniker]
      ) extends AbstractPersistedMonadicKVDBNode[KVDBNodeRequest,KVDBNodeResponse](
	cache, acquaintances
      ) {    
	override def toString() : String = {
	  (
	    this.getClass.getName.split( "\\." ).last + "@"
	    + ( name match { case MURI( uri ) => uri; case _ => name } )
	  )
	}
	def wrapResponse(
	  msrc : Moniker, dreq : Msgs.DReq, rsrc : mTT.Resource
	) : FramedMsg = {
	  frameResponse( msrc )(
	    dreq match {
	      case Msgs.MDGetRequest( path ) => {
		RsrcMsgs.MDGetResponseRsrc[Namespace,Var,Tag,Value]( path, mTT.portRsrc( rsrc, path ) )	  
	      }
	      case Msgs.MDFetchRequest( path ) => {
		RsrcMsgs.MDFetchResponseRsrc[Namespace,Var,Tag,Value]( path, mTT.portRsrc( rsrc, path ) )
	      }
	      case Msgs.MDSubscribeRequest( path ) => {
		RsrcMsgs.MDSubscribeResponseRsrc[Namespace,Var,Tag,Value]( path, mTT.portRsrc( rsrc, path ) )
	      }
	      case _ => {
		throw new Exception( "unexpected request type " + dreq )
	      }	  
	    }
	  )
	}
	
	def wrapResponse(
	  msrc : Moniker, dreq : Msgs.DReq
	) : FramedMsg = {
	  frameResponse( msrc )(
	    dreq match {	  
	      case Msgs.MDPutRequest( path, _ ) => {
		RsrcMsgs.MDPutResponseRsrc[Namespace,Var,Tag,Value]( path )
	      }
	      case Msgs.MDPublishRequest( path, _ ) => {
		RsrcMsgs.MDPublishResponseRsrc[Namespace,Var,Tag,Value]( path )
	      }
	      case _ => {
		throw new Exception( "unexpected request type " + dreq )
	      }
	    }
	  )
	}
	
	def handleValue( dreq : Msgs.DReq, oV : Option[mTT.Resource], msrc : Moniker ) : Unit = {
	  for( q <- stblQMap.get( msrc ); value <- oV ) {	
	    tweet( ( this + " sending value " + oV + " back " ) )	   
	    q ! wrapResponse( msrc, dreq, value )
	  }
	}
	
	def dispatchDMsg( dreq : FramedMsg ) : Unit = {
	  dreq match {
	    case Left( JustifiedRequest( msgId, mtrgt, msrc, lbl, body, _ ) ) => {
	      body match {
		case dgreq@Msgs.MDGetRequest( path ) => {	  
		  tweet( ( this + " getting locally for location : " + path ) )
		  reset {
		    for( v <- get( List( msrc ) )( false )( path ) ) {
		      tweet(
			(
			  this 
			  + " returning from local get for location : "
			  + path
			  + "\nwith value : " + v
			)
		      )
		      handleValue( dgreq, v, msrc )
		    }
		  }
		}
		
		case dfreq@Msgs.MDFetchRequest( path ) => {
		  tweet( ( this + "fetching locally for location : " + path ) )
		  reset {
		    for( v <- fetch( List( msrc ) )( false )( path ) ) {
		      tweet(
			(
			  this 
			  + " returning from local fetch for location : "
			  + path
			  + "\nwith value : " + v
			)
		      )
		      handleValue( dfreq, v, msrc )
		    }
		  }
		}
		
		case dsreq@Msgs.MDSubscribeRequest( path ) => {
		  tweet( ( this + "subscribing locally for location : " + path ) )
		  reset {
		    for( v <- subscribe( List( msrc ) )( path ) ) {
		      tweet(
			(
			  this 
			  + " returning from local subscribe for location : "
			  + path
			  + "\nwith value : " + v
			)
		      )
		      handleValue( dsreq, v, msrc )
		    }
		  }
		}
		
		case dpreq@Msgs.MDPutRequest( path, value ) => {	
		  reset { cache.put( path, mTT.Ground( value ) ) }
		  for( q <- stblQMap.get( msrc ) ) {
		    q ! wrapResponse( msrc, dpreq )
		  }
		}
		case dpbreq@Msgs.MDPublishRequest( path, value ) => {	
		  reset { cache.publish( path, mTT.Ground( value ) ) }
		  for( q <- stblQMap.get( msrc ) ) {
		    q ! wrapResponse( msrc, dpbreq )
		  }
		}
	      }
	    }
	    case Right( JustifiedResponse( msgId, mtrgt, msrc, lbl, body, _ ) ) => {
	      body match {
		case RsrcMsgs.MDGetResponseRsrc( path, rsrc ) => {
		  reset { cache.put( path, rsrc ) }
		}
		case RsrcMsgs.MDFetchResponseRsrc( path, rsrc ) => {
		  reset { cache.put( path, rsrc ) }
		}
		case RsrcMsgs.MDSubscribeResponseRsrc( path, rsrc ) => {
		  reset { cache.publish( path, rsrc ) }
		}	    
		case dput : RsrcMsgs.MDPutResponse[Namespace,Var,Tag,Value] => {	
		}
		case _ => {
		  tweet(
		    (
		      this 
		      + " handling unexpected message : "
		      + body
		    )
		  )
		}
	      }
	    }
	  }      
	  
	}
	
	def mkGetRsp( path : CnxnCtxtLabel[Namespace,Var,Tag], rsrc : mTT.Resource ) = {
	  RsrcMsgs.MDGetResponseRsrc[Namespace,Var,Tag,Value]( path, rsrc )
	}
	
	def dispatchDMsgs()  : Unit = {
	  reset {
	    for( dreq <- ??() ) {
	      tweet( this + " handling : " + dreq )	
	      dispatchDMsg( dreq )
	    }
	  }
	}
	
	def forward(
	  ask : dAT.AskNum,
	  hops : List[Moniker],
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	) : Unit = {
	  
	  tweet( ( this + " in forwardGet with hops: " + hops ) )
	  
	  for( trgt <- acquaintances; q <- stblQMap.get( trgt ) if !hops.contains( trgt ) ) {	
	    val request : KVDBNodeRequest = 
	      ask match {
		case dAT.AGetNum => {
		  Msgs.MDGetRequest[Namespace,Var,Tag,Value](
		    path
		  )
		}
		case dAT.AFetchNum => {
		  Msgs.MDFetchRequest[Namespace,Var,Tag,Value](
		    path
		  )
		}
		case dAT.ASubscribeNum => {
		  Msgs.MDSubscribeRequest[Namespace,Var,Tag,Value](
		    path
		  )
		}
	      }
	    
	    val framedReq = frameRequest( trgt )( request )
	    tweet( ( this + " forwarding " + framedReq + " to " + trgt ) )
	    q ! framedReq
	  }
	}
	
	def mget( ask : dAT.AskNum, hops : List[Moniker] )(
	  channels : Map[mTT.GetRequest,mTT.Resource],
	  registered : Map[mTT.GetRequest,List[RK]],
	  consume : Boolean,
	  cursor : Boolean
	)(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {        
	  Generator {
	    rk : ( Option[mTT.Resource] => Unit @suspendable ) =>
	      shift {
		outerk : ( Unit => Unit ) =>
		  reset {
		    for(
		      oV <- cache.mget( channels, registered, consume )( path ) 
		    ) {
		      oV match {
			case None => {
			  tweet( ">>>>> forwarding..." )
			  forward( ask, hops, path )
			  rk( oV )
			}
			case _ => rk( oV )
		      }
		    }
		  }
	      }
	  }      
	}
	
	def get( hops : List[Moniker] )( cursor : Boolean )(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	) : Generator[Option[mTT.Resource],Unit,Unit] = {              
	  mget( dAT.AGetNum, hops )( cache.theMeetingPlace, cache.theWaiters, true, cursor )( path )    
	}
	
	def get( cursor : Boolean )(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	): Generator[Option[mTT.Resource],Unit,Unit] = {
	  get( Nil )( cursor )( path )
	}    
	
	override def get(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	) : Generator[Option[mTT.Resource],Unit,Unit] = {        
	  get( Nil )( false )( path )    
	}
	
	def fetch( hops : List[Moniker] )(
	  cursor : Boolean
	)(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {              
	  mget( dAT.AFetchNum, hops )(
	    cache.theMeetingPlace, cache.theWaiters, false, cursor
	  )( path )    
	}
	
	def fetch(
	  cursor : Boolean
	)(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {
	  get( Nil )( cursor )( path )
	}
	
	override def fetch(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {        
	  fetch( Nil )( false )( path )    
	}
	
	def subscribe( hops : List[Moniker] )(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {        
	  mget( dAT.ASubscribeNum, hops )(
	    cache.theChannels, cache.theSubscriptions, true, false
	  )( path )    
	}
	
	override def subscribe(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {        
	  subscribe( Nil )( path )    
	}
	
	override def put( ptn : CnxnCtxtLabel[Namespace,Var,Tag], rsrc : mTT.Resource ) = {
	  mput( cache.theMeetingPlace, cache.theWaiters, false )( ptn, rsrc )
	}
	override def publish( ptn : CnxnCtxtLabel[Namespace,Var,Tag], rsrc : mTT.Resource ) = {
	  mput( cache.theChannels, cache.theSubscriptions, true )( ptn, rsrc )
	}
	
	override def configFileName : Option[String] = None
	override def configurationDefaults : ConfigurationDefaults = {
	  ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
	} 
      }
      
      object KVDBNodeFactory extends AMQPURIOps with FJTaskRunners {
	def ptToPt( here : URI, there : URI ) : PersistedMonadicKVDBNode = {
	  val node = PersistedMonadicKVDBNode( PersistedMonadicKVDB( MURI( here ) ), List( MURI( there ) ) )
	  spawn { node.dispatchDMsgs() }
	  node
	}
	def loopBack( here : URI ) : PersistedMonadicKVDBNode = {
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
	  val node = PersistedMonadicKVDBNode( PersistedMonadicKVDB( MURI( hereNow ) ), List( MURI( thereNow ) ) )
	  spawn { node.dispatchDMsgs() }
	  node
	}
      }
    }
}
