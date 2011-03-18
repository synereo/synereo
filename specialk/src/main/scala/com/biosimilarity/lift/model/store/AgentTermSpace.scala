// -*- mode: Scala;-*- 
// Filename:    AgentTermSpace.scala 
// Authors:     lgm                                                    
// Creation:    Thu Mar  3 12:37:15 2011 
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

trait AgentTermStoreScope[Namespace,Var,Tag,Value] 
extends MonadicTermStoreScope[Namespace,Var,Tag,Value] {
   case class AgentCnxn(
      override val src : URI,
      override val label : String,
      override val trgt : URI
    ) extends Cnxn[URI,String,URI]

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
    )( cnxn : AgentCnxn ) : Unit = {
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
		+ " in coll : " + pd.xmlCollStr( cnxn )
	      )
	    )
	    store( pd.xmlCollStr( cnxn ) )( rcrd )
	  }
	}
      }
    }

     def putPlaces( persist : Option[PersistenceDescriptor] )(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      ptn : mTT.GetRequest,
      rsrc : mTT.Resource
    )( cnxn : AgentCnxn ) : Generator[PlaceInstance,Unit,Unit] = {    
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
	      )(
		cnxn
	      )
	    }
	  }
      }
    }
    
    def mput( persist : Option[PersistenceDescriptor] )(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : Boolean
    )( cnxn : AgentCnxn )(
      ptn : mTT.GetRequest,
      rsrc : mTT.Resource
    ) : Unit @suspendable = {    
      for(
	placeNRKsNSubst
	<- putPlaces(
	  persist
	)( channels, registered, ptn, rsrc )( cnxn )
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
	    )( cnxn )
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
    )( cnxn : AgentCnxn )(
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
				  )( pd.xmlCollStr( cnxn ) )
				) {
				  val srvc : Service =
				    getQueryService( xmlColl )(
				      pd.queryServiceType,
				      pd.queryServiceVersion
				    );
				  tweet(
				    (
				      "querying db : " + pd.db
				      + " from coll " + pd.xmlCollStr( cnxn )
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
    
    def put( cnxn : AgentCnxn )(
      ptn : mTT.GetRequest, rsrc : mTT.Resource
    ) =
      mput( persistenceDescriptor )(
	theMeetingPlace, theWaiters, false
      )( cnxn )( ptn, rsrc )
    def publish( cnxn : AgentCnxn )(
      ptn : mTT.GetRequest, rsrc : mTT.Resource
    ) =
      mput( persistenceDescriptor )(
	theChannels, theSubscriptions, true
      )( cnxn )( ptn, rsrc )

    def get( hops : List[URI] )(
      cnxn : AgentCnxn
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      mget( persistenceDescriptor, AGet, hops )(
	theMeetingPlace, theWaiters, true
      )( cnxn )( path )    
    }
    def get(
      cnxn : AgentCnxn
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      get( Nil )( cnxn )( path )    
    }

    def fetch( hops : List[URI] )(
      cnxn : AgentCnxn
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      mget( persistenceDescriptor, AFetch, hops )(
	theMeetingPlace, theWaiters, false
      )( cnxn )( path )    
    }
    def fetch(
      cnxn : AgentCnxn
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      fetch( Nil )( cnxn )( path )    
    }

    def subscribe( hops : List[URI] )(
      cnxn : AgentCnxn
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      mget( persistenceDescriptor, ASubscribe, hops )(
	theChannels, theSubscriptions, true
      )( cnxn )( path )    
    }
    def subscribe(
      cnxn : AgentCnxn
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      subscribe( Nil )( cnxn )( path )    
    }
  }
}

object AgentTS
 extends AgentTermStoreScope[String,String,String,String] 
  with UUIDOps {
    import AgentURIDefaults._
    type MTTypes = MonadicTermTypes[String,String,String,String]
    object TheMTT extends MTTypes
    override def protoTermTypes : MTTypes = TheMTT
    
    val aLabel =
      new CnxnCtxtLeaf[String,String,String](
	Left(
	  "a"
	)
      )
    val bLabel =
      new CnxnCtxtLeaf[String,String,String](
	Left(
	  "b"
	)
      )
    
    val cLabel =
      new CnxnCtxtLeaf[String,String,String](
	Left(
	  "c"
	)
      )
    val dLabel =
      new CnxnCtxtLeaf[String,String,String](
	Left(
	  "d"
	)
      )
    
    lazy val Mona = new MonadicTermStore()
    def Imma( a : String, b : String )  =
      new DistributedMonadicGeneratorJunction( a, List( b ) )
    
    class PersistedtedStringMGJ(
      override val name : URI,
      override val acquaintances : Seq[URI]
    ) extends PersistedMonadicGeneratorJunction(
      name, acquaintances
    ) {
      class StringExistDescriptor
      extends ExistDescriptor( database ) {
	override def xmlCollStr : String = {
	  throw new Exception( "use Cnxn-based interface instead" )
	}
	override def xmlCollStr[Src,Label,Trgt](
	  cnxn : Cnxn[Src,Label,Trgt]
	) : String = {     
	  cnxn match {
	    case agentCnxn : AgentCnxn =>
	      agentCnxn.src.getHost + agentCnxn.trgt.getHost
	    case _ =>
	      throw new Exception( "unexpected cnxn type" )
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
	  new StringExistDescriptor
	)
    }
    
    def Pimma( a : String, b : String )  =
      new PersistedtedStringMGJ( a, List( b ) )

    import scala.collection.immutable.IndexedSeq

    def testQuery( pimmgJunq : PersistedtedStringMGJ )(
      cnxn : AgentCnxn
    )(
      qryStr : String
    ) : Option[IndexedSeq[org.xmldb.api.base.Resource]] = {
      for(
	pd <- pimmgJunq.persistenceDescriptor;
	xmlColl <- pimmgJunq.getCollection( true )( pd.xmlCollStr( cnxn ) )
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
