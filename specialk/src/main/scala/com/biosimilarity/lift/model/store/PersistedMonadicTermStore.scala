// -*- mode: Scala;-*- 
// Filename:    PersistedMonadicTermStore.scala 
// Authors:     lgm                                                    
// Creation:    Fri Mar 18 15:04:22 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._

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

import javax.xml.transform.OutputKeys
import java.util.Properties
import java.net.URI
import java.io.File
import java.io.FileInputStream
import java.io.OutputStreamWriter

trait PersistedTermStoreScope[Namespace,Var,Tag,Value] 
extends MonadicTermStoreScope[Namespace,Var,Tag,Value] {  
  trait PersistenceManifest {    
    def db : Database
    def storeUnitStr[Src,Label,Trgt]( cnxn : Cnxn[Src,Label,Trgt] ) : String
    def storeUnitStr : String
    def toFile( ptn : mTT.GetRequest ) : Option[File]
    def query( ptn : mTT.GetRequest ) : Option[String]

    def labelToNS : Option[String => Namespace]
    def textToVar : Option[String => Var]
    def textToTag : Option[String => Tag]        

    def kvNameSpace : Namespace

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

    def asCacheValue(
      ccl : CnxnCtxtLabel[Namespace,Var,String]
    ) : Value    

    def asCacheValue(
      ltns : String => Namespace,
      ttv : String => Var,
      value : Elem
    ) : Option[Value]

    def asResource(
      key : mTT.GetRequest, // must have the pattern to determine bindings
      value : Elem
    ) : Option[mTT.Resource]
    
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

    def asResource(
      key : mTT.GetRequest, // must have the pattern to determine bindings
      value : Elem
    ) : Option[mTT.Resource] = {
      for( pd <- persistenceManifest; rsrc <- pd.asResource( key, value ) )
	yield { rsrc }
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
  }

  abstract class XMLDBManifest(
    override val db : Database
  ) extends PersistenceManifest 
    with CnxnXQuery[Namespace,Var,Tag]
  with CnxnXML[Namespace,Var,Tag]
  with CnxnCtxtInjector[Namespace,Var,Tag]
  with XMLIfy[Namespace,Var]
  with Blobify
  with UUIDOps {
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

    override def asStoreRecord(
      key : mTT.GetRequest,
      value : mTT.Resource
    ) : CnxnCtxtLabel[Namespace,Var,String] with Factual = {
      new CnxnCtxtBranch[Namespace,Var,String](
	kvNameSpace,
	List( asStoreKey( key ), asStoreValue( value ) )
      )
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

    override def asResource(
      key : mTT.GetRequest, // must have the pattern to determine bindings
      value : Elem
    ) : Option[mTT.Resource] = {
      for(
	ltns <- labelToNS;
	ttv <- textToVar;
	ttt <- textToTag;
	vCCL <- asCacheValue( ltns, ttv, value )	
      ) yield {
	// BUGBUG -- LGM need to return the Solution
	// Currently the PersistenceManifest has no access to the
	// unification machinery
	mTT.RBound( 
	  Some( mTT.Ground( vCCL ) ),
	  None
	)
      }
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
    override val name : URI,
    override val acquaintances : Seq[URI]
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
      BaseXDefaults.asInstanceOf[ConfigurationDefaults]
    } 
    
    // BUGBUG -- LGM: Further evidence of problem with current factorization...
    override def asResource(
      key : mTT.GetRequest, // must have the pattern to determine bindings
      value : Elem
    ) : Option[mTT.Resource] = {
      for(
	ltns <- labelToNS;
	ttv <- textToVar;
	vCCL <- asCacheValue( ltns, ttv, value )	
      ) yield {
	// BUGBUG -- LGM need to return the Solution
	// Currently the PersistenceManifest has no access to the
	// unification machinery
	mTT.RBound( 
	  Some( mTT.Ground( vCCL ) ),
	  None
	)
      }
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
		tweet(
		  "namespace mismatch: " + kvNameSpace + "," + ns
		)
		None
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

    def putPlaces( persist : Option[PersistenceManifest] )(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      ptn : mTT.GetRequest,
      rsrc : mTT.Resource,
      collName : Option[String]
    ) : Generator[PlaceInstance,Unit,Unit] = {    
      Generator {
	k : ( PlaceInstance => Unit @suspendable ) => 
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
		k( itr.next )
	      }
	    }
	    // No...
	    case Nil => {
	      // Store the rsrc at a representative of the ptn
	      tweet( "no waiters waiting for a value at " + ptn )
	      //channels( representative( ptn ) ) = rsrc
	      putInStore(
		persist, channels, ptn, None, rsrc, collName
	      )
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
	)( channels, registered, ptn, rsrc, collName )
      ) {
	val PlaceInstance( wtr, Right( rks ), s ) = placeNRKsNSubst
	tweet( "waiters waiting for a value at " + wtr + " : " + rks )
	rks match {
	  case rk :: rrks => {	
	    if ( consume ) {
	      for( sk <- rks ) {
		spawn {
		  sk( s( rsrc ) )
		}
	      }
	    }
	    else {
	      registered( wtr ) = rrks
	      rk( s( rsrc ) )
	    }
	  }
	  case Nil => {
	    putInStore(
	      persist, channels, ptn, Some( wtr ), rsrc, collName
	    )
	  }
	}
      }
      
    }    
    
    def mget(
      persist : Option[PersistenceManifest],
      ask : dAT.Ask,
      hops : List[URI]
    )(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : Boolean,
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
			  tweet( ">>>>> forwarding..." )
			  forward( ask, hops, path )
			  rk( oV )
			}
			case Some( pd ) => {
			  tweet(
			    "accessing db : " + pd.db
			  )
			  val oQry = query( path )
			  oQry match {
			    case None => {
			      tweet( ">>>>> forwarding..." )
			      forward( ask, hops, path )
			      rk( oV )
			    }
			    case Some( qry ) => {
			      val xmlCollName =
				collName.getOrElse(
				  storeUnitStr.getOrElse(
				    bail()
				  )
				)
			      getCollection( true )(
				xmlCollName
			      )
			      match {
				case Some( xmlColl ) => {
				  val srvc =
				    getQueryService( xmlColl )(
				      queryServiceType,
				      queryServiceVersion
				    )
				
				  tweet(
				    (
				      "querying db : " + pd.db
				      + " from coll " + xmlCollName
				      + " where " + qry
				    )
				  )
				
				  val rsrcSet =
				    execute( xmlColl )( srvc )( qry )
				
				  if ( rsrcSet.getSize == 0 ) {	
				    tweet(
				      (
					"database "
					+ xmlColl.getName
					+ " had no matching resources."
				      )
				    )
				    forward( ask, hops, path )
				    rk( oV )
				  }
				  else {
				    tweet(
				      (
					"database "
					+ xmlColl.getName
					+ " had "
					+ rsrcSet.getSize
					+ " matching resources."
				      )
				    )
				    // BUGBUG -- LGM need to mput rcrds
				    // and delete rcrd from DB
				  
				    val rsrcIter = rsrcSet.getIterator
				    
				    val rcrds =
				      new MutableList[( Elem, Option[mTT.Resource] )]()
				  
				    while( rsrcIter.hasMoreResources ) {
				      val xrsrc = rsrcIter.nextResource	  
				      
				      val xrsrcCntntStr =
				      xrsrc.getContent.toString

				      tweet( "retrieved " + xrsrcCntntStr )
				      
				      val ersrc : Elem =
					XML.loadString( xrsrcCntntStr )
				      
				      rcrds += (( ersrc, asResource( path, ersrc ) ))
				    }
				    
				    val ( ersrc, rslt ) = rcrds( 0 )
				    val cacheRcrds = rcrds.drop( 1 )
				    for( ( elem, cacheRcrd ) <- cacheRcrds ) {
				      tweet( "caching " + cacheRcrd )
				    }
				    
				    tweet( "removing from store " + rslt )
				    removeFromStore( 
				      persist,
				      ersrc,
				      collName
				    )
				    tweet( "returning " + rslt )
				    rk( rslt )
				  }
				}
				case _ => {
				  forward( ask, hops, path )
				  rk( oV )
				}
			      }
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
	  executeInSession( 
	    List( deletionQry ),
	    ostrm
	  )
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
	theMeetingPlace, theWaiters, false, xmlCollName
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

    override def get( hops : List[URI] )(
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
	theMeetingPlace, theWaiters, true, xmlCollName
      )( path )    
    }
    override def get(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      get( Nil )( path )    
    }

    override def fetch( hops : List[URI] )(
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
	theMeetingPlace, theWaiters, false, xmlCollName
      )( path )    
    }
    override def fetch(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      fetch( Nil )( path )    
    }

    override def subscribe( hops : List[URI] )(
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
	theChannels, theSubscriptions, true, xmlCollName
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


/* ------------------------------------------------------------------
 * Mostly self-contained object to support unit testing
 * ------------------------------------------------------------------ */ 

object PersistedMonadicTS
 extends PersistedTermStoreScope[String,String,String,String] 
  with UUIDOps {
    import SpecialKURIDefaults._
    import CnxnLeafAndBranch._

    type MTTypes = MonadicTermTypes[String,String,String,String]
    object TheMTT extends MTTypes
    override def protoTermTypes : MTTypes = TheMTT

    type DATypes = DistributedAskTypes
    object TheDAT extends DATypes
    override def protoAskTypes : DATypes = TheDAT
    
    class PersistedtedStringMGJ(
      val dfStoreUnitStr : String,
      override val name : URI,
      override val acquaintances : Seq[URI]
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
	override def storeUnitStr[Src,Label,Trgt](
	  cnxn : Cnxn[Src,Label,Trgt]
	) : String = {     
	  cnxn match {
	    case CCnxn( s, l, t ) =>
	      s.toString + l.toString + t.toString
	  }
	}	

	def kvNameSpace : String = "record"

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
	  valueStorageType match {
	    case "CnxnCtxtLabel" => {
	      tweet(
		"warning: CnxnCtxtLabel method is using XStream"
	      )
	      new CnxnCtxtLeaf[String,String,String](
		Left[String,String](
		  new XStream( new JettisonMappedXmlDriver ).toXML( rsrc )
		)
	      )
	    }
	    case "XStream" => {
	      tweet(
		"using XStream method"
	      )
	      val blob =
		new XStream( new JettisonMappedXmlDriver ).toXML( rsrc )
	      //asXML( rsrc )
	      new CnxnCtxtLeaf[String,String,String](
		Left[String,String]( blob )
	      )
	    }
	    case _ => {
	      throw new Exception( "unexpected value storage type" )
	    }
	  }	  
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
	      fromBlob(
		rv.replace(
		  "&quot;",
		  "\""
		)
	      ) match {
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
      
      }

      def persistenceManifest : Option[PersistenceManifest] = {
	val sid = Some( ( s : String ) => s )
	Some(
	  new StringXMLDBManifest( dfStoreUnitStr, sid, sid, sid )
	)
      }
    }
    
    def ptToPt( storeUnitStr : String, a : String, b : String )  = {
      new PersistedtedStringMGJ( storeUnitStr, a, List( b ) )
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

    type MTTypes = MonadicTermTypes[Symbol,Symbol,Any,Any]
    object TheMTT extends MTTypes
    override def protoTermTypes : MTTypes = TheMTT

    type DATypes = DistributedAskTypes
    object TheDAT extends DATypes
    override def protoAskTypes : DATypes = TheDAT
    
    class PersistedtedStdMGJ(
      val dfStoreUnitStr : String,
      override val name : URI,
      override val acquaintances : Seq[URI]
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
	override def storeUnitStr[Src,Label,Trgt](
	  cnxn : Cnxn[Src,Label,Trgt]
	) : String = {     
	  cnxn match {
	    case CCnxn( s, l, t ) =>
	      s.toString + l.toString + t.toString
	  }
	}	

	def kvNameSpace : Symbol = 'record

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
	  valueStorageType match {
	    case "CnxnCtxtLabel" => {
	      tweet(
		"warning: CnxnCtxtLabel method is using XStream"
	      )
	      new CnxnCtxtLeaf[Symbol,Symbol,String](
		Left[String,Symbol](
		  new XStream( new JettisonMappedXmlDriver ).toXML( rsrc )
		)
	      )
	    }
	    case "XStream" => {
	      tweet(
		"using XStream method"
	      )
	      val blob =
		new XStream( new JettisonMappedXmlDriver ).toXML( rsrc )
	      //asXML( rsrc )
	      new CnxnCtxtLeaf[Symbol,Symbol,String](
		Left[String,Symbol]( blob )
	      )
	    }
	    case _ => {
	      throw new Exception( "unexpected value storage type" )
	    }
	  }	  
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
		fromBlob(
		  rv.replace(
		    "&quot;",
		    "\""
		  )
		) match {
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
      
      }

      def persistenceManifest : Option[PersistenceManifest] = {
	val sid = Some( ( s : String ) => s )
	val sym = Some( ( s : String ) => Symbol( s ) )
	Some(
	  new StringXMLDBManifest( dfStoreUnitStr, sym, sym, sid )
	)
      }
    }
    
    def ptToPt( storeUnitStr : String, a : String, b : String )  = {
      new PersistedtedStdMGJ( storeUnitStr, a, List( b ) )
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
