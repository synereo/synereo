// -*- mode: Scala;-*- 
// Filename:    PersistedMonadicTermStore.scala 
// Authors:     lgm                                                    
// Creation:    Fri Mar 18 15:04:22 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------


package com.biosimilarity.lift.model.store

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

import org.prolog4j._

import org.exist.storage.DBBroker

import org.xmldb.api.base.{ Resource => XmlDbRrsc, _}
import org.xmldb.api.modules._
import org.xmldb.api._

import org.exist.util.serializer.SAXSerializer
import org.exist.util.serializer.SerializerPool

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
  abstract class PersistedMonadicGeneratorJunction(
    override val name : URI,
    override val acquaintances : Seq[URI]
  ) extends DistributedMonadicGeneratorJunction(
    name,
    acquaintances
  ) with KVTrampoline {    
    def putInStore(
      persist : Option[PersistenceDescriptor],
      channels : Map[mTT.GetRequest,mTT.Resource],
      ptn : mTT.GetRequest,
      wtr : Option[mTT.GetRequest],
      rsrc : mTT.Resource
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
	    val rcrd = pd.asRecord( ptn, rsrc )
	    tweet(
	      (
		"storing to db : " + pd.db
		+ " pair : " + rcrd
		+ " in coll : " + pd.xmlCollStr
	      )
	    )
	    store( pd.xmlCollStr )( rcrd )
	  }
	}
      }
    }

     def putPlaces( persist : Option[PersistenceDescriptor] )(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      ptn : mTT.GetRequest,
      rsrc : mTT.Resource
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
		persist, channels, ptn, None, rsrc 
	      )
	    }
	  }
      }
    }
    
    def mput( persist : Option[PersistenceDescriptor] )(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : Boolean
    )(
      ptn : mTT.GetRequest,
      rsrc : mTT.Resource
    ) : Unit @suspendable = {    
      for(
	placeNRKsNSubst
	<- putPlaces(
	  persist
	)( channels, registered, ptn, rsrc )
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
	      persist, channels, ptn, Some( wtr ), rsrc 
	    )
	  }
	}
      }
      
    }
    def mget(
      persist : Option[PersistenceDescriptor],
      ask : Ask,
      hops : List[URI]
    )(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : Boolean
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
			  forward( ask, hops, path )
			  rk( oV )
			}
			case Some( pd ) => {
			  tweet(
			    "accessing db : " + pd.db
			  )
			  val query = pd.query( path )
			  query match {
			    case None => {
			      forward( ask, hops, path )
			    }
			    case Some( qry ) => {
			      spawn {
				for(			    
				  xmlColl <- getCollection(
				    true
				  )( pd.xmlCollStr )
				) {
				  val srvc : Service =
				    getQueryService( xmlColl )(
				      pd.queryServiceType,
				      pd.queryServiceVersion
				    );
				  tweet(
				    (
				      "querying db : " + pd.db
				      + " from coll " + pd.xmlCollStr
				      + " where " + qry
				    )
				  )
				  val rsrcSet =
				    execute( xmlColl )( srvc )( qry )
				  if ( rsrcSet.getSize == 0 ) {
				    forward( ask, hops, path )
				  }
				  else {
				    // TBD
				    // Need to put results
				  }
				}
			      }
			    }
			    rk( oV )
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
    
    override def put(
      ptn : mTT.GetRequest, rsrc : mTT.Resource
    ) =
      mput( persistenceDescriptor )(
	theMeetingPlace, theWaiters, false
      )( ptn, rsrc )
    override def publish(
      ptn : mTT.GetRequest, rsrc : mTT.Resource
    ) =
      mput( persistenceDescriptor )(
	theChannels, theSubscriptions, true
      )( ptn, rsrc )

    override def get( hops : List[URI] )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      mget( persistenceDescriptor, AGet, hops )(
	theMeetingPlace, theWaiters, true
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
      mget( persistenceDescriptor, AFetch, hops )(
	theMeetingPlace, theWaiters, false
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
      mget( persistenceDescriptor, ASubscribe, hops )(
	theChannels, theSubscriptions, true
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
    
    lazy val Mona = new MonadicTermStore()
    def Imma( a : String, b : String )  =
      new DistributedMonadicGeneratorJunction( a, List( b ) )
    
    class PersistedtedStringMGJ(
      val xmlCollStr : String,
      override val name : URI,
      override val acquaintances : Seq[URI]
    ) extends PersistedMonadicGeneratorJunction(
      name, acquaintances
    ) {
      class StringExistDescriptor(
	override val xmlCollStr : String
      )
      extends ExistDescriptor( database ) {
	override def xmlCollStr[Src,Label,Trgt](
	  cnxn : Cnxn[Src,Label,Trgt]
	) : String = {     
	  cnxn match {
	    case CCnxn( s, l, t ) =>
	      s.toString + l.toString + t.toString
	  }
	}	

	def kvNameSpace : String = "KVPairs"
	def asValue(
	  rsrc : mTT.Resource
	) : CnxnCtxtLeaf[String,String,String] = {
	  val blob =
	    new XStream( new JettisonMappedXmlDriver ).toXML( rsrc )
	  //asXML( rsrc )
	  new CnxnCtxtLeaf[String,String,String](
	    Left[String,String]( blob )
	  )
	}
      
      }

      def persistenceDescriptor : Option[PersistenceDescriptor] =
	Some(
	  new StringExistDescriptor( xmlCollStr )
	)
    }
    
    def Pimma( xmlcstr : String, a : String, b : String )  =
      new PersistedtedStringMGJ( xmlcstr, a, List( b ) )

    import scala.collection.immutable.IndexedSeq

    def testQuery( pimmgJunq : PersistedtedStringMGJ )(
      qryStr : String
    ) : Option[IndexedSeq[org.xmldb.api.base.Resource]] = {
      for(
	pd <- pimmgJunq.persistenceDescriptor;
	xmlColl <- pimmgJunq.getCollection( true )( pd.xmlCollStr )
      )
      yield {
	val xqSrvc =
	  pimmgJunq.getQueryService( xmlColl )(
	    "XQueryService", "1.0"
	  ).asInstanceOf[XQueryService]

	val rsrcSet = xqSrvc.execute( xqSrvc.compile( qryStr ) )
	println( "number of results = " + rsrcSet.getSize )

	val outputProperties : Properties = new Properties()
        outputProperties.setProperty(
	  OutputKeys.INDENT, "yes"
	)
	
	val serializer : SAXSerializer =	  
	  SerializerPool.getInstance().borrowObject(
            Class.forName( "org.exist.util.serializer.SAXSerializer" )
	  ).asInstanceOf[SAXSerializer]	

	val rslt = 
	  for( i <- 0 to rsrcSet.getSize.toInt - 1 )
	  yield {
	    val rsrc = rsrcSet.getResource( i )
	    val xmlRsrc = rsrc.asInstanceOf[XMLResource]

	    val bufStrm = new java.io.ByteArrayOutputStream()

	    serializer.setOutput(
	      new OutputStreamWriter(bufStrm),
	      outputProperties
	    )	    

	    xmlRsrc.getContentAsSAX( serializer )
	    
	    println( "the resource is : " + bufStrm )

	    rsrc
	  }

	SerializerPool.getInstance().returnObject(serializer);
	
	rslt
      }
    }
    
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
