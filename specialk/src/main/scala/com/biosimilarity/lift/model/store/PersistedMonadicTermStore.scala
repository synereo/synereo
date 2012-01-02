// -*- mode: Scala;-*- 
// Filename:    PersistedMonadicTermStore.scala 
// Authors:     lgm                                                    
// Creation:    Fri Mar 18 15:04:22 2011 
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

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.util.continuations._ 
import scala.xml._
import scala.collection.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MutableList

import org.prolog4j._

//import org.exist.storage.DBBroker

import org.xmldb.api.base.{ Resource => XmlDbRrsc, _}
import org.xmldb.api.modules._
import org.xmldb.api._

//import org.exist.util.serializer.SAXSerializer
//import org.exist.util.serializer.SerializerPool

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import biz.source_code.base64Coder.Base64Coder

import javax.xml.transform.OutputKeys
import java.util.Properties
import java.net.URI
import java.io.File
import java.io.FileInputStream
import java.io.OutputStreamWriter
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayOutputStream

trait PersistedTermStoreScope[Namespace,Var,Tag,Value] 
extends MonadicTermStoreScope[Namespace,Var,Tag,Value] {  
  trait PersistenceScope
    extends ExcludedMiddleScope[mTT.GetRequest,mTT.GetRequest,mTT.Resource] {
      trait PersistenceManifest {    
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
	) : emT.PlaceInstance //Option[mTT.Resource]
	
	def recordDeletionQueryTemplate : String = {
	  "delete node let $key := %RecordKeyConstraints% for $rcrd in collection( '%COLLNAME%' )//record where deep-equal($rcrd/*[1], $key) return $rcrd"
	}
      }
      
      trait PersistenceManifestTrampoline {
	def persistenceManifest : Option[PersistenceManifest]
	
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
	) : Option[emT.PlaceInstance] /* Option[mTT.Resource] */ = {
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
	override val db : Database
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
		valueStorageType match {
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
  
      abstract class PersistedMonadicGeneratorJunction(
	//override val name : URI,
	override val name : Moniker,
	//override val acquaintances : Seq[URI]
	override val acquaintances : Seq[Moniker]
      ) extends DistributedMonadicGeneratorJunction(
	name,
	acquaintances
      ) with PersistenceManifestTrampoline
	       with BaseXXMLStore           
	       with BaseXCnxnStorage[Namespace,Var,Tag]           
      {    	
	override def tmpDirStr : String = {
	  val tds = config.getString( "storageDir", "tmp" )       
	  val tmpDir = new java.io.File( tds )
	  if ( ! tmpDir.exists ) {
	    tmpDir.mkdir
	  }
	  tds
	}
	
	override def configurationDefaults : ConfigurationDefaults = {
	  ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
	} 
	
	def asCursor(
	  values : List[mTT.Resource]
	) : Option[mTT.Resource] = {
	  
	  val ig : mTT.Generator[mTT.Resource, Unit, Unit]  = mTT.itergen[mTT.Resource]( values )
	  
	  // BUGBUG -- LGM need to return the Solution
	  // Currently the PersistenceManifest has no access to the
	  // unification machinery
	  Some (
            mTT.RBound(
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
	    //val ks : Option[List[Option[mTT.Resource] => Unit @suspendable]] = None
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
	  //hops : List[URI]
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
			      tweet( ">>>>> no persistence manifest..." )
			      tweet( ">>>>> forwarding..." )
			      forward( ask, hops, path )
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
				      tweet( ">>>>> unable to compile query for path " + path )
				      tweet( ">>>>> forwarding..." )
				      forward( ask, hops, path )
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
					  forward( ask, hops, path )
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
						//val ersrc : Option[mTT.Resource] = asResource( path, rslt )
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
				  tweet( ">>>>> forwarding..." )
				  forward( ask, hops, path )
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
	
	override def put(
	  ptn : mTT.GetRequest, rsrc : mTT.Resource
	) = {
	  val perD = persistenceManifest
	  val xmlCollName = 
	    perD match {
	      case None => None
	      case Some( pd ) => Some( pd.storeUnitStr )
	    }
	  mput( perD )(
	    theMeetingPlace, theWaiters, true, xmlCollName
	  )( ptn, rsrc )
	}
	override def publish(
	  ptn : mTT.GetRequest, rsrc : mTT.Resource
	) = {
	  val perD = persistenceManifest
	  val xmlCollName = 
	    perD match {
	      case None => None
	      case Some( pd ) => Some( pd.storeUnitStr )
	    }
	  mput( perD )(
	    theChannels, theSubscriptions, true, xmlCollName
	  )( ptn, rsrc )
	}
	
	override def get( hops : List[Moniker] )(
	  cursor: Boolean
	)(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {        
	  val perD = persistenceManifest
	  val xmlCollName = 
	    perD match {
	      case None => None
	      case Some( pd ) => Some( pd.storeUnitStr )
	    }
	  mget( perD, dAT.AGet, hops )(
	    theMeetingPlace, theWaiters, true, cursor, xmlCollName
	  )( path )    
	}
	override def get(
	  cursor: Boolean
	)(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {
	  get( Nil )( cursor )( path )
	}
	override def get(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {        
	  get( Nil )( false )( path )    
	}
	
	override def fetch( hops : List[Moniker] )(
	  cursor: Boolean
	)(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {        
	  val perD = persistenceManifest
	  val xmlCollName = 
	    perD match {
	      case None => None
	      case Some( pd ) => Some( pd.storeUnitStr )
	    }
	  mget( perD, dAT.AFetch, hops )(
	    theMeetingPlace, theWaiters, false, cursor, xmlCollName
	  )( path )    
	}
	override def fetch(
	  cursor: Boolean
	)(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {
	  fetch( Nil )( cursor )( path )
	}
	override def fetch(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {        
	  fetch( Nil )( false )( path )    
	}
	
	override def subscribe( hops : List[Moniker] )(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {        
	  val perD = persistenceManifest
	  val xmlCollName = 
	    perD match {
	      case None => None
	      case Some( pd ) => Some( pd.storeUnitStr )
	    }
	  mget( perD, dAT.ASubscribe, hops )(
	    theChannels, theSubscriptions, true, false, xmlCollName
	  )( path )    
	}
	override def subscribe(
	  path : CnxnCtxtLabel[Namespace,Var,Tag]
	)
	: Generator[Option[mTT.Resource],Unit,Unit] = {        
	  subscribe( Nil )( path )    
	}
      }      
    }
}


package usage {
/* ------------------------------------------------------------------
 * Mostly self-contained object to support unit testing
 * ------------------------------------------------------------------ */ 

object PersistedMonadicTS
 extends PersistedTermStoreScope[String,String,String,String] 
  with UUIDOps {
    import SpecialKURIDefaults._
    import CnxnLeafAndBranch._
    import identityConversions._

    type MTTypes = MonadicTermTypes[String,String,String,String]
    object TheMTT extends MTTypes
    override def protoTermTypes : MTTypes = TheMTT

    type DATypes = DistributedAskTypes
    object TheDAT extends DATypes
    override def protoAskTypes : DATypes = TheDAT

    object Being extends PersistenceScope {      
      
      override type EMTypes = ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]

      object theEMTypes
	       extends ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]
      {
	case class PrologSubstitution( soln : Solution[String] )
	   extends Function1[mTT.Resource,Option[mTT.Resource]] {
	     override def apply( rsrc : mTT.Resource ) = {
	       Some( mTT.RBound( Some( rsrc ), Some( soln ) ) )
	     }
	   }
	override type Substitution = PrologSubstitution
      }      

      override def protoEMTypes : EMTypes =
	theEMTypes

      class PersistedStringMGJ(
	val dfStoreUnitStr : String,
	//override val name : URI,
	override val name : Moniker,
	//override val acquaintances : Seq[URI]
	override val acquaintances : Seq[Moniker]
      ) extends PersistedMonadicGeneratorJunction(
	name, acquaintances
      ) {      
	class StringXMLDBManifest(
	  override val storeUnitStr : String,
	  override val labelToNS : Option[String => String],
	  override val textToVar : Option[String => String],
	  override val textToTag : Option[String => String]        
	)
	extends XMLDBManifest( database ) {
	  override def valueStorageType : String = {
	    throw new Exception( "valueStorageType not overriden in instantiation" )
	  }
	  override def continuationStorageType : String = {
	    throw new Exception( "continuationStorageType not overriden in instantiation" )
	  }

	  override def storeUnitStr[Src,Label,Trgt](
	    cnxn : Cnxn[Src,Label,Trgt]
	  ) : String = {     
	    cnxn match {
	      case CCnxn( s, l, t ) =>
		s.toString + l.toString + t.toString
	    }	    
	  }	
	  
	  def kvNameSpace : String = "record"
	  def kvKNameSpace : String = "kRecord"
	  
	  def compareNameSpace( ns1 : String, ns2 : String ) : Boolean = {
	    ns1.equals( ns2 )
	  }
	  
	  // BUGBUG -- LGM: Evidence of a problem with this factorization
	  override def asCacheValue(
	    ltns : String => String,
	    ttv : String => String,
	    value : Elem
	  ) : Option[String] = {
	    tweet(
	      "Shouldn't be here!"
	    )
	    None
	  }
	  
	  override def asStoreValue(
	    rsrc : mTT.Resource
	  ) : CnxnCtxtLeaf[String,String,String] with Factual = {
	    val blob =
	      valueStorageType match {
		case "Base64" => {
		  val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
		  val oos : ObjectOutputStream = new ObjectOutputStream( baos )
		  oos.writeObject( rsrc )
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
	  ) : String = {
	    tweet(
	      "converting to cache value"
	    )
	    //asPatternString( ccl )
	    ccl match {
	      case CnxnCtxtBranch(
		"String",
		CnxnCtxtLeaf( Left( rv ) ) :: Nil
	      ) => {
		val unBlob =
		  fromXQSafeJSONBlob( rv )
		
		unBlob match {
		  case rsrc : mTT.Resource => {
		    getGV( rsrc ).getOrElse( "" )
		  }
		}
	      }
	      case _ => {
		asPatternString( ccl )
	      }
	    }
	  }
	 
	  override def asResource(
	    key : mTT.GetRequest, // must have the pattern to determine bindings
	    value : Elem
	  ) : emT.PlaceInstance /* Option[mTT.Resource] */ = {
	    val ttt = ( x : String ) => x
	    
	    val ptn = asPatternString( key )
	    println( "ptn : " + ptn )		
	    
	    val oRsrc : Option[emT.PlaceInstance] =
	      for(
		ltns <- labelToNS;
		ttv <- textToVar;
		//ttt <- textToTag;
		ccl <- xmlIfier.fromXML( ltns, ttv, ttt )( value )
	      ) yield {
		ccl match {
		  case CnxnCtxtBranch( ns, k :: v :: Nil ) => {
		    val oGvOrK = 
		      (if ( compareNameSpace( ns, kvNameSpace ) ) {	    
			// BUGBUG -- LGM need to return the Solution
			// Currently the PersistenceManifest has no access to the
			// unification machinery	      
			
			for ( vCCL <- asCacheValue( ltns, ttv, value ) ) 
			yield {				    
			  Left[mTT.Resource,mTT.Resource]( mTT.Ground( vCCL ) )
			}
		      }
		       else {
			 if ( compareNameSpace( ns, kvKNameSpace ) ) {
			   Some( Right[mTT.Resource,mTT.Resource]( asCacheK( v ) ) )
			 }
			 else {
			   throw new Exception( "unexpected namespace : (" + ns + ")" )
			 }
		       });

		    val cclKey =
		      xmlIfier.fromXML( ltns, ttv, ttt )(
			xmlIfier.asXML( key )
		      ) match {
			case Some( cclX ) => cclX
			case _ => throw new Exception( "xml roundtrip failed " + key )
		      }

		    val soln = 
		      unifyQuery(
			asPatternString(
			  cclKey
			),
			asPatternString( k )
		      )
		    emT.PlaceInstance(
		      k,
		      oGvOrK match {
			case Some( Left( gv ) ) => {
			  Left[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( gv )
			}
			case Some( Right( mTT.Continuation( ks ) ) ) => {
			  Right[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( ks )
			}
			case _ => {
			  throw new Exception( "excluded middle contract broken: " + oGvOrK )
			}
		      },
		      // BUGBUG -- lgm : why can't the compiler determine
		      // that this cast is not necessary?
		      theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
		    )
		  }
		  case _ => {
		    throw new Exception( "unexpected record format : " + value )
		  }
		}      
	      }
	    
	    // BUGBUG -- lgm : this is a job for flatMap
	    //oRsrc.getOrElse( None )
	    oRsrc match {
	      case Some( pI ) => {
		pI
	      }
	      case _ => {
		throw new Exception( "violated excluded middle : " + oRsrc )
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
	  //asPatternString( ccl )	
	  ccl match {
	    case CnxnCtxtBranch(
	      "String",
	      CnxnCtxtLeaf( Left( rv ) ) :: Nil
	    ) => {
	      val unBlob =
		fromXQSafeJSONBlob( rv )
	      
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
	  value : Elem
	) : Option[mTT.Continuation] = {
	  throw new Exception( "shouldn't be calling this version of asCacheK" )
	}	
	
	def persistenceManifest : Option[PersistenceManifest] = {
	  val sid = Some( ( s : String ) => s )
	  val kvdb = this;
	  Some(
	    new StringXMLDBManifest( dfStoreUnitStr, sid, sid, sid ) {
	      override def valueStorageType : String = {
		kvdb.valueStorageType
	      }
	      override def continuationStorageType : String = {
		kvdb.valueStorageType
	      }
	    }
	  )
	}
	
	/*
	 override def cnxnTrgtTermStr( trgt : Either[String,String] )
	 : String = {
	 (
	 trgt match {
	 case Left( t ) => {
	 t match {
	 case s : String => {
	 "\"" + s + "\""
	 }
	 case _ => t
	 }
	 }
	 case Right( v ) => cnxnVarTermStr( v )
	 }
	 )
	 }
	 */	
	
      }
      
    }
    
    import Being._

    def singleton( storeUnitStr : String, a : String )  = {
      new PersistedStringMGJ( storeUnitStr, a, List( ) )
    }

    def ptToPt( storeUnitStr : String, a : String, b : String )  = {
      new PersistedStringMGJ( storeUnitStr, a, List( b ) )
    }

    def loopBack( storeUnitStr : String ) = {
      ptToPt( storeUnitStr, "localhost", "localhost" )
    }

    import scala.collection.immutable.IndexedSeq
        
    type MsgTypes = DTSMSH[String,String,String,String]   
    
    val protoDreqUUID = getUUID()
    val protoDrspUUID = getUUID()    
    
    object MonadicDMsgs extends MsgTypes {
      
      override def protoDreq : DReq = MDGetRequest( aLabel )
      override def protoDrsp : DRsp = MDGetResponse( aLabel, aLabel.toString )
      override def protoJtsreq : JTSReq =
	JustifiedRequest(
	  protoDreqUUID,
	  new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	  new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	  getUUID(),
	  protoDreq,
	  None
	)
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
    
    override def protoMsgs : MsgTypes = MonadicDMsgs
  }

object StdPersistedMonadicTS
 extends PersistedTermStoreScope[Symbol,Symbol,Any,Any] 
  with UUIDOps {
    import SpecialKURIDefaults._
    import CnxnLeafAndBranch._
    import CCLDSL._
    import identityConversions._

    type MTTypes = MonadicTermTypes[Symbol,Symbol,Any,Any]
    object TheMTT extends MTTypes
    override def protoTermTypes : MTTypes = TheMTT

    type DATypes = DistributedAskTypes
    object TheDAT extends DATypes
    override def protoAskTypes : DATypes = TheDAT

    object Being extends PersistenceScope {

      override type EMTypes = ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]

      object theEMTypes
	       extends ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]
      {
	case class PrologSubstitution( soln : Solution[String] )
	   extends Function1[mTT.Resource,Option[mTT.Resource]] {
	     override def apply( rsrc : mTT.Resource ) = {
	       Some( mTT.RBound( Some( rsrc ), Some( soln ) ) )
	     }
	   }
	override type Substitution = PrologSubstitution
      }      

      override def protoEMTypes : EMTypes =
	theEMTypes
    
      class PersistedStdMGJ(
	val dfStoreUnitStr : String,
	//override val name : URI,
	override val name : Moniker,
	//override val acquaintances : Seq[URI]
	override val acquaintances : Seq[Moniker]
      ) extends PersistedMonadicGeneratorJunction(
	name, acquaintances
      ) {
	class StringXMLDBManifest(
	  override val storeUnitStr : String,
	  override val labelToNS : Option[String => Symbol],
	  override val textToVar : Option[String => Symbol],
	  override val textToTag : Option[String => Any]        
	)
	extends XMLDBManifest( database ) {
	  override def valueStorageType : String = {
	    throw new Exception( "valueStorageType not overriden in instantiation" )
	  }
	  override def continuationStorageType : String = {
	    throw new Exception( "continuationStorageType not overriden in instantiation" )
	  }
	  override def storeUnitStr[Src,Label,Trgt](
	    cnxn : Cnxn[Src,Label,Trgt]
	  ) : String = {     
	    cnxn match {
	      case CCnxn( s, l, t ) =>
		s.toString + l.toString + t.toString
	    }
	  }	
	  
	  def kvNameSpace : Symbol = 'record
	  def kvKNameSpace : Symbol = 'kRecord
	  
	  def compareNameSpace( ns1 : Symbol, ns2 : Symbol ) : Boolean = {
	    ns1.equals( ns2 )
	  }
	  
	  // BUGBUG -- LGM: Evidence of a problem with this factorization
	  override def asCacheValue(
	    ltns : String => Symbol,
	    ttv : String => Symbol,
	    value : Elem
	  ) : Option[String] = {
	    tweet(
	      "Shouldn't be here!"
	    )
	    None
	  }
	  
	  override def asStoreValue(
	    rsrc : mTT.Resource
	  ) : CnxnCtxtLeaf[Symbol,Symbol,String] with Factual = {
	    val blob = 
	      valueStorageType match {
		case "Base64" => {
		  val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
		  val oos : ObjectOutputStream = new ObjectOutputStream( baos )
		  oos.writeObject( rsrc )
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
	    new CnxnCtxtLeaf[Symbol,Symbol,String](
	      Left[String,Symbol](
		blob
	      )
	    )
	  }
	  
	  def asCacheValue(
	    ccl : CnxnCtxtLabel[Symbol,Symbol,String]
	  ) : String = {
	    tweet(
	      "converting to cache value"
	    )
	    //asPatternString( ccl )
	    ccl match {
	      case CnxnCtxtBranch(
		storeType,
		CnxnCtxtLeaf( Left( rv ) ) :: Nil
	      ) => {
		def extractValue( rv : String ) : String = {
		  val unBlob =
		    fromXQSafeJSONBlob( rv )
		  
		  unBlob match {
		    case rsrc : mTT.Resource => {
		      (getGV( rsrc ).getOrElse( "" ) + "")
		    }
		  }
		}
		
		(storeType + "") match {
		  case "String" => {
		    extractValue( rv )
		  }
		  case "'String" => {
		    extractValue( rv )
		  }
		}	      
	      }
	      case _ => {
		asPatternString(
		  ccl.asInstanceOf[CnxnCtxtLabel[Symbol,Symbol,Any]]
		)
	      }
	    }
	  }
	 
	  override def asResource(
	    key : mTT.GetRequest, // must have the pattern to determine bindings
	    value : Elem
	  ) : emT.PlaceInstance /* Option[mTT.Resource] */ = {
	    throw new Exception( "not yet implemented" )
	  }
 
	}
	
	def persistenceManifest : Option[PersistenceManifest] = {
	  val sid = Some( ( s : String ) => s )
	  val sym = Some( ( s : String ) => Symbol( s ) )
	  val kvdb = this;
	  Some(
	    new StringXMLDBManifest( dfStoreUnitStr, sym, sym, sid ) {
	      override def valueStorageType : String = {
		kvdb.valueStorageType
	      }
	      override def continuationStorageType : String = {
		kvdb.valueStorageType
	      }
	    }
	  )
	}
      }
      
    }

    import Being._
    
    def ptToPt( storeUnitStr : String, a : String, b : String )  = {
      new PersistedStdMGJ( storeUnitStr, a, List( b ) )
    }

    def loopBack( storeUnitStr : String ) = {
      ptToPt( storeUnitStr, "localhost", "localhost" )
    }

    import scala.collection.immutable.IndexedSeq
        
    type MsgTypes = DTSMSH[Symbol,Symbol,Any,Any]   
    
    val protoDreqUUID = getUUID()
    val protoDrspUUID = getUUID()    
    
    object MonadicDMsgs extends MsgTypes {
      
      override def protoDreq : DReq = 
	MDGetRequest( $('protoDReq)( "yo!" ) )
      override def protoDrsp : DRsp =
	MDGetResponse( $('protoDRsp)( "oy!" ), Symbol( aLabel.toString ) )
      override def protoJtsreq : JTSReq =
	JustifiedRequest(
	  protoDreqUUID,
	  new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	  new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	  getUUID(),
	  protoDreq,
	  None
	)
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
    
    override def protoMsgs : MsgTypes = MonadicDMsgs
  }

}
