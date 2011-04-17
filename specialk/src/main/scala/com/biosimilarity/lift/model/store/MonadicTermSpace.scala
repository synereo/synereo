// -*- mode: Scala;-*- 
// Filename:    MonadicTermSpace.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 31 01:46:38 2011 
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

trait MonadicTermTypes[Namespace,Var,Tag,Value] {
  trait Resource
  case class Ground( v : Value ) extends Resource
  case class RMap(
    m : TMapR[Namespace,Var,Tag,Value]
  ) extends Resource
  case class RBound(
    rsrc : Option[Resource], soln : Option[Solution[String]]
  ) extends Resource

  type GetRequest = CnxnCtxtLabel[Namespace,Var,Tag]  

  class TMapR[Namespace,Var,Tag,Value]
  extends HashMap[GetRequest,Resource]  
  
}

trait MonadicTermTypeScope[Namespace,Var,Tag,Value] {
  type MTTypes <: MonadicTermTypes[Namespace,Var,Tag,Value]
  def protoTermTypes : MTTypes
  val mTT : MTTypes = protoTermTypes
  def asCCL(
    gReq : mTT.GetRequest
  ) : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
    gReq.asInstanceOf[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
  }
  implicit def toValue( v : Value ) = mTT.Ground( v )
}

trait DistributedAskTypes {
  trait Ask
  case object AGet extends Ask
  case object AFetch extends Ask
  case object ASubscribe extends Ask    
  
  // workaround due to bug in scala runtime
  type AskNum = Int
  val AGetNum : AskNum = 0
  val AFetchNum : AskNum = 1
  val ASubscribeNum : AskNum = 2
}

trait DistributedAskTypeScope {
  type DATypes <: DistributedAskTypes
  def protoAskTypes : DATypes
  val dAT : DATypes = protoAskTypes
}

trait MonadicTermStoreScope[Namespace,Var,Tag,Value] 
extends MonadicTermTypeScope[Namespace,Var,Tag,Value] 
  with MonadicDTSMsgScope[Namespace,Var,Tag,Value]
  with DistributedAskTypeScope {          

  class MonadicTermStore(
  )
  extends MonadicTupleSpace[mTT.GetRequest,mTT.GetRequest,mTT.Resource] 
  with CnxnCtxtInjector[Namespace,Var,Tag]
  with CnxnUnificationCompositeTermQuery[Namespace,Var,Tag]
  with CnxnConversions[Namespace,Var,Tag]
  with WireTap
  with Journalist
  with ConfiggyReporting
  //with ConfiggyJournal
  with ConfiguredJournal
  with ConfigurationTrampoline
  with UUIDOps {
    override def tap [A] ( fact : A ) : Unit = {
      reportage( fact )
    }
    
    override val theMeetingPlace =
      new mTT.TMapR[Namespace,Var,Tag,Value]()
    override val theChannels =
      new mTT.TMapR[Namespace,Var,Tag,Value]()
    override val theWaiters =
      new TMapK[Namespace,Var,Tag,Value]()
    override val theSubscriptions =
      new TMapK[Namespace,Var,Tag,Value]()

    class TMapK[Namespace,Var,Tag,Value]
    extends HashMap[mTT.GetRequest,List[RK]]

    case class PrologSubstitution( soln : Solution[String] )
	 extends Function1[mTT.Resource,Option[mTT.Resource]] {
	   override def apply( rsrc : mTT.Resource ) = {
	     Some( mTT.RBound( Some( rsrc ), Some( soln ) ) )
	   }
	 }

    override type Substitution = PrologSubstitution
    
    override def representative(
      ptn : mTT.GetRequest
    ) : mTT.GetRequest = {
      ptn
    }
    override def fits(
      ptn : mTT.GetRequest,
      place : mTT.GetRequest
    ) : Boolean = {
      //println( "in fits on " + this )
      matches( ptn, place ) match {
	case Some( soln ) => {
	  //PrologSubstitution( soln )
	  true
	}
	case None => {
	  false
	}
      }
    }

    override def fitsK(
      ptn : mTT.GetRequest,
      place : mTT.GetRequest
    ) : Option[Substitution] = {
      //println( "in fitsK on " + this )
      val matchRslts = matches( ptn, place )
      //println( "match results in fitsK on " + this + " are " + matchRslts )
      matchRslts match {
	case Some( soln : Solution[String] ) => {
	  Some( PrologSubstitution( soln ) )
	}
	case None => {
	  None
	}
      }
    }

    def getGV( rsrc : mTT.Resource ) : Option[Value] = {
      rsrc match {
	case mTT.Ground( v ) => Some( v )
	case mTT.RBound( Some( nrsrc ), _ ) => getGV( nrsrc )
	case _ => None
      }
    }

    override def configFileName : Option[String] = None
    override def configurationDefaults : ConfigurationDefaults = {
      ConfiguredJournalDefaults.asInstanceOf[ConfigurationDefaults]
    }
 
  }

  abstract class MonadicGeneratorJunction(
    override val name : URI,
    override val acquaintances : Seq[URI],
    override val requests : ListBuffer[Msgs.JTSReq],
    override val responses : ListBuffer[Msgs.JTSRsp],
    override val nameSpace :
    Option[LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]],
    override val traceMonitor : TraceMonitor[Msgs.DReq,Msgs.DRsp]
  )
  extends MonadicTermStore(
  ) with MonadicCollective
  with MonadicJSONAMQPDispatcher[Msgs.JTSReqOrRsp]
  with MonadicWireToTrgtConversion
  with MonadicGenerators {
    override def toString() : String = {
      name + " -> " + acquaintances
    }

    override type Wire = String
    override type Trgt = Msgs.JTSReqOrRsp

    override lazy val agentTwistedPairs
    : Map[URI,SemiMonadicAgentJSONAMQPTwistedPair[String]] =
      meetNGreet( acquaintances )

    def forward(
      ask : dAT.Ask,
      hops : List[URI],
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Unit = {

      tweet(
	( this + " in forwardGet with hops: " + hops )
      )

      // Dummy declarations to avoid a bug in the scala runtime
      val das = ask
      val dasClass = ask.getClass
      
      for(
	( uri, jsndr ) <- agentTwistedPairs
	if !hops.contains( uri )
      ) {
	tweet(
	  ( this + " forwarding to " + uri )
	)
	val smajatp : SMAJATwistedPair =
	  jsndr.asInstanceOf[SMAJATwistedPair]
	
	smajatp.send(
	  ask match {
	    case dAT.AGet => {
	      Msgs.MDGetRequest[Namespace,Var,Tag,Value](
		path
	      ).asInstanceOf[Msgs.DReq]
	    }
	    case dAT.AFetch => {
	      Msgs.MDFetchRequest[Namespace,Var,Tag,Value](
		path
	      ).asInstanceOf[Msgs.DReq]
	    }
	    case dAT.ASubscribe => {
	      Msgs.MDSubscribeRequest[Namespace,Var,Tag,Value](
		path
	      ).asInstanceOf[Msgs.DReq]
	    }
	  }
	)
      }
    }
    
    def forward(
      ask : dAT.AskNum,
      hops : List[URI],
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Unit = {

      tweet(
	( this + " in forwardGet with hops: " + hops )
      )

      for(
	( uri, jsndr ) <- agentTwistedPairs
	if !hops.contains( uri )
      ) {
	tweet(
	  ( this + " forwarding to " + uri )
	)
	val smajatp : SMAJATwistedPair =
	  jsndr.asInstanceOf[SMAJATwistedPair]
	
	smajatp.send(
	  ask match {
	    case dAT.AGetNum => {
	      Msgs.MDGetRequest[Namespace,Var,Tag,Value](
		path
	      ).asInstanceOf[Msgs.DReq]
	    }
	    case dAT.AFetchNum => {
	      Msgs.MDFetchRequest[Namespace,Var,Tag,Value](
		path
	      ).asInstanceOf[Msgs.DReq]
	    }
	    case dAT.ASubscribeNum => {
	      Msgs.MDSubscribeRequest[Namespace,Var,Tag,Value](
		path
	      ).asInstanceOf[Msgs.DReq]
	    }
	  }
	)
      }
    }
  }

  class DistributedMonadicGeneratorJunction(
    override val name : URI,
    override val acquaintances : Seq[URI]
  ) extends MonadicGeneratorJunction(
    name,
    acquaintances,
    new ListBuffer[Msgs.JTSReq](),
    new ListBuffer[Msgs.JTSRsp](),
    Some( new LinkedHashMap[URI,Socialite[Msgs.DReq,Msgs.DRsp]]() ),
    AnAMQPTraceMonitor
  ) with QueueNameVender {
    def sendRsp(
      atp : SemiMonadicAgentJSONAMQPTwistedPair[String],
      dreq : Msgs.DReq,	
      oGv : Option[Value]
    ) = {
      val smajatp : SMAJATwistedPair =
	atp.asInstanceOf[SMAJATwistedPair]
      
      smajatp.send(
	dreq match {
	  case Msgs.MDGetRequest( path ) => {
	    oGv match {
	      case Some( gv ) => {
		Msgs.MDGetResponse[Namespace,Var,Tag,Value](
		  path,
		  gv
		)
	      }
	      case None => {
		throw new Exception( "get must take value" )
	      }
	    }
	  }
	  case Msgs.MDFetchRequest( path ) => {
	    oGv match {
	      case Some( gv ) => {
		Msgs.MDFetchResponse[Namespace,Var,Tag,Value](
		  path,
		  gv
		)
	      }
	      case None => {
		throw new Exception( "fetch must take value" )
	      }
	    }
	  }
	  case Msgs.MDSubscribeRequest( path ) => {
	    oGv match {
	      case Some( gv ) => {
		Msgs.MDSubscribeResponse[Namespace,Var,Tag,Value](
		  path,
		  gv
		)
	      }
	      case None => {
		throw new Exception( "subscribe must take value" )
	      }
	    }
	  }
	  case Msgs.MDPutRequest( path, _ ) => {
	    Msgs.MDPutResponse[Namespace,Var,Tag,Value](
	      path
	    )
	  }
	  case Msgs.MDPublishRequest( path, _ ) => {
	    Msgs.MDPublishResponse[Namespace,Var,Tag,Value](
	      path
	    )
	  }
	}
      )
    }

    def handleValue(
      dreq : Msgs.DReq,
      oV : Option[mTT.Resource],
      msrc : URI
    ) : Unit = {
      //tap( v )            

      for(
	atp <- agentTwistedPairs.get( msrc );
	value <- oV
      ) {	

	value match {
	  case mTT.RBound(
	    Some( mTT.Ground( gv ) ),
	    Some( soln ) 
	  ) => {
	    tweet(
	      (
		this + " sending value " + oV + " back "
	      )
	    )
	    
	    sendRsp( atp, dreq, Some( gv ) )
	      
	  }

	  case mTT.RBound(
	    Some( mTT.Ground( gv ) ),
	    None 
	  ) => {
	    tweet(
	      (
		this + " sending value " + oV + " back "
	      )
	    )
	    
	    sendRsp( atp, dreq, Some( gv ) )
	      
	  }

	  case mTT.Ground( gv ) => {
	    tweet(
	      (
		this + " sending value " + oV + " back "
	      )
	    )
	    
	    sendRsp( atp, dreq, Some( gv ) )

	  }
	  case _ => {
	    tweet(
	      (
		this 
		+ " not sending composite value " + oV
		+ " back "
	      )
	    )
	  }
	}
      }
      oV
    }

    def handleRequest( dreq : Msgs.JTSReq ) : Unit = {      
      val JustifiedRequest( 
	msgId, mtrgt, msrc, lbl, body, _
      ) = dreq

      tweet( this + "handling : " + dreq	)

      body match {
	case dgreq@Msgs.MDGetRequest( path ) => {	  
	  tweet(
	    ( this + "getting locally for location : " + path )
	  )
	  reset {
	    for( v <- get( List( msrc ) )( path ) ) {
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
	  tweet(
	    ( this + "fetching locally for location : " + path )
	  )
	  reset {
	    for( v <- fetch( List( msrc ) )( path ) ) {
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
	  tweet(
	    ( this + "fetching locally for location : " + path )
	  )
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
	  reset { put( path, mTT.Ground( value ) ) }
	  for( atp <- agentTwistedPairs.get( msrc ) ) {
	    sendRsp( atp, dpreq, None )
	  }
	}
	case dpbreq@Msgs.MDPublishRequest( path, value ) => {	
	  reset { publish( path, mTT.Ground( value ) ) }
	  for( atp <- agentTwistedPairs.get( msrc ) ) {
	    sendRsp( atp, dpbreq, None )
	  }
	}
      }
    }
    
    def handleResponse( drsp : Msgs.JTSRsp ) : Unit = {      
      val JustifiedResponse( 
	  msgId, mtrgt, msrc, lbl, body, _
      ) = drsp

      body match {
	case Msgs.MDGetResponse( path, value ) => {
	  reset { put( path, mTT.Ground( value ) ) }
	}
	case Msgs.MDFetchResponse( path, value ) => {
	  reset { put( path, mTT.Ground( value ) ) }
	}
	case Msgs.MDSubscribeResponse( path, value ) => {
	  reset { publish( path, mTT.Ground( value ) ) }
	}
	case dput : Msgs.MDPutResponse[Namespace,Var,Tag,Value] => {	
	}
	case dpub : Msgs.MDPublishResponse[Namespace,Var,Tag,Value] => {	
	}
	case _ => {
	  tweet(
	    (
	      this 
	      + " handling unexpected message : " + body
	    )
	  )
	}
      }
    }
    
    def handleIncoming( dmsg : Msgs.JTSReqOrRsp ) : Unit = {
      dmsg match {
	case Left(
	  dreq@JustifiedRequest( 
	    msgId, mtrgt, msrc, lbl, body, _
	  )
	) => {
	  tweet(
	    (
	      this + " handling : " + dmsg
	      + " from " + msrc
	      + " on behalf of " + mtrgt
	    )
	  )
	  handleRequest( dreq )
	}
	case Right(
	  drsp@JustifiedResponse( 
	    msgId, mtrgt, msrc, lbl, body, _
	  )
	) => {
	  tweet(
	    (
	      this + " handling : " + dmsg
	      + " from " + msrc
	      + " on behalf of " + mtrgt
	    )
	  )
	  handleResponse( drsp )
	}
      }
    }

    def mget( ask : dAT.Ask, hops : List[URI] )(
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

    def mget( ask : dAT.AskNum, hops : List[URI] )(
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
  
    def get( hops : List[URI] )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      
      // Dummy declarations to avoid a bug in the scala runtime
      // val das = dAT.AGet
//       val dasClass = dAT.AGet.getClass
      
      //mget( dAT.AGet, hops )(
      mget( dAT.AGetNum, hops )(
	theMeetingPlace, theWaiters, true
      )( path )    
    }

    override def get(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      get( Nil )( path )    
    }

    def getValue(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Generator[Value,Unit,Unit] = 
      Generator {
	k : ( Value => Unit @suspendable ) =>
	  for(
	    orsrc <- get( path )
	    //rsrc <- orsrc
	    //gv <- getGV( rsrc )
	  ) {
	    orsrc match {
	      case Some( rsrc ) => {
		getGV( rsrc ) match {
		  case Some( gv ) => k( gv )
		  case None =>
		}
	      }
	      case None => 
	    };
	  }
      }

    def fetch( hops : List[URI] )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      
      // Dummy declarations to avoid a bug in the scala runtime
      // val das = dAT.AFetch
//       val dasClass = dAT.AFetch.getClass

      //mget( dAT.AFetch, hops )(
      mget( dAT.AFetchNum, hops )(
	theMeetingPlace, theWaiters, false
      )( path )    
    }

    override def fetch(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      fetch( Nil )( path )    
    }

    def fetchValue(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Generator[Value,Unit,Unit] = 
      Generator {
	k : ( Value => Unit @suspendable ) =>
	  for(
	    orsrc <- fetch( path )
	    //rsrc <- orsrc
	    //gv <- getGV( rsrc )
	  ) {
	    orsrc match {
	      case Some( rsrc ) => {
		getGV( rsrc ) match {
		  case Some( gv ) => k( gv )
		  case None =>
		}
	      }
	      case None => 
	    };
	  }
      }

    def subscribe( hops : List[URI] )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        

      // Dummy declarations to avoid a bug in the scala runtime
      // val das = dAT.ASubscribe
//       val dasClass = dAT.ASubscribe.getClass

      //mget( dAT.ASubscribe, hops )(
      mget( dAT.ASubscribeNum, hops )(
	theChannels, theSubscriptions, true
      )( path )    
    }

    override def subscribe(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      subscribe( Nil )( path )    
    }
    
    def subscribeValue(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Generator[Value,Unit,Unit] = 
      Generator {
	k : ( Value => Unit @suspendable ) =>
	  for(
	    orsrc <- subscribe( path )
	    //rsrc <- orsrc
	    //gv <- getGV( rsrc )
	  ) {
	    orsrc match {
	      case Some( rsrc ) => {
		getGV( rsrc ) match {
		  case Some( gv ) => k( gv )
		  case None =>
		}
	      }
	      case None => 
	    };
	  }
      }
  }
   
}

/* ------------------------------------------------------------------
 * Mostly self-contained object to support unit testing
 * ------------------------------------------------------------------ */ 

object MonadicTS
 extends MonadicTermStoreScope[String,String,String,String] 
  with UUIDOps {
    import SpecialKURIDefaults._
    import CnxnLeafAndBranch._

    type MTTypes = MonadicTermTypes[String,String,String,String]
    object TheMTT extends MTTypes
    override def protoTermTypes : MTTypes = TheMTT

    type DATypes = DistributedAskTypes
    object TheDAT extends DATypes
    override def protoAskTypes : DATypes = TheDAT
    
    lazy val Mona = new MonadicTermStore()
    def ptToPt( a : String, b : String )  =
      new DistributedMonadicGeneratorJunction( a, List( b ) )    
    def loopBack() = {
      ptToPt( "localhost", "localhost" )
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

