// -*- mode: Scala;-*- 
// Filename:    MonadicKVDBNode.scala 
// Authors:     lgm                                                    
// Creation:    Mon Feb 27 18:51:20 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._
import net.liftweb.amqp._

import scala.util.continuations._ 
import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.collection.mutable.Map
import scala.collection.mutable.MapProxy
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

import com.rabbitmq.client._

import org.prolog4j._

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.util.UUID
import java.net.URI
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream

trait MonadicKVDBNodeScope[Namespace,Var,Tag,Value]
extends MonadicSoloTermStoreScope[Namespace,Var,Tag,Value] 
{
  trait DTSMSHRsrc extends DTSMSH[Namespace,Var,Tag,Value] {
    trait RsrcResponse[Namespace,Var,Tag,Value]
    case class MDGetResponseRsrc[Namespace,Var,Tag,Value](
      path : CnxnCtxtLabel[Namespace,Var,Tag],
      rsrc : mTT.Resource
    ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
	 with RsrcResponse[Namespace,Var,Tag,Value]
    case class MDFetchResponseRsrc[Namespace,Var,Tag,Value](
      path : CnxnCtxtLabel[Namespace,Var,Tag],
      rsrc : mTT.Resource
    ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
	 with RsrcResponse[Namespace,Var,Tag,Value]
    case class MDSubscribeResponseRsrc[Namespace,Var,Tag,Value](
      path : CnxnCtxtLabel[Namespace,Var,Tag],
      rsrc : mTT.Resource
    ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
	 with RsrcResponse[Namespace,Var,Tag,Value]
    case class MDPutResponseRsrc[Namespace,Var,Tag,Value](
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
	 with RsrcResponse[Namespace,Var,Tag,Value]
    case class MDPublishResponseRsrc[Namespace,Var,Tag,Value](
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) extends MDistributedTermSpaceResponse[Namespace,Var,Tag,Value]
	 with RsrcResponse[Namespace,Var,Tag,Value]    
  }

  type RsrcMsgTypes <: DTSMSHRsrc
  override type MsgTypes = RsrcMsgTypes
  def protoRsrcMsgs : RsrcMsgTypes
  val RsrcMsgs : RsrcMsgTypes = protoRsrcMsgs

  type KVDBNodeRequest = Msgs.MDistributedTermSpaceRequest[Namespace,Var,Tag,Value]
  type KVDBNodeResponse = RsrcMsgs.RsrcResponse[Namespace,Var,Tag,Value]

  abstract class AbstractMonadicKVDB[ReqBody, RspBody](
    override val name : Moniker
  ) extends Individual[ReqBody,RspBody,AbstractMonadicKVDBNode](
    name,
    new ListBuffer[JustifiedRequest[ReqBody,RspBody]](),
    new ListBuffer[JustifiedResponse[ReqBody,RspBody]]()
  ) with MonadicTermStoreT
  {    
  }
  
  abstract class AbstractMonadicKVDBNode[ReqBody, RspBody](
    val localCache : AbstractMonadicKVDB[ReqBody,RspBody],
    override val acquaintances : List[Moniker]
  ) extends MonadicTxPortFramedMsgDispatcher[String,ReqBody,RspBody,AbstractMonadicKVDBNode](
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

  case class MonadicKVDB(
    override val name : Moniker
  ) extends AbstractMonadicKVDB[KVDBNodeRequest,KVDBNodeResponse](
    name
  ) {
    override def configFileName : Option[String] = None
    override def configurationDefaults : ConfigurationDefaults = {
      ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
    } 
  }
  case class MonadicKVDBNode(
    cache : MonadicKVDB,
    override val acquaintances : List[Moniker]
  ) extends AbstractMonadicKVDBNode[KVDBNodeRequest,KVDBNodeResponse](
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
    def ptToPt( here : URI, there : URI ) : MonadicKVDBNode = {
      val node = MonadicKVDBNode( MonadicKVDB( MURI( here ) ), List( MURI( there ) ) )
      spawn { node.dispatchDMsgs() }
      node
    }
    def loopBack( here : URI ) : MonadicKVDBNode = {
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
      val node = MonadicKVDBNode( MonadicKVDB( MURI( hereNow ) ), List( MURI( thereNow ) ) )
      spawn { node.dispatchDMsgs() }
      node
    }
  }
}

package usage {
  object MonadicKVDBNet
       extends MonadicKVDBNodeScope[String,String,String,Double]
       with UUIDOps
  {
    import SpecialKURIDefaults._
    import identityConversions._

    type MTTypes = MonadicTermTypes[String,String,String,Double]
    object TheMTT extends MTTypes with Serializable
    override def protoTermTypes : MTTypes = TheMTT

    type DATypes = DistributedAskTypes
    object TheDAT extends DATypes with Serializable
    override def protoAskTypes : DATypes = TheDAT
    
    override type MsgTypes = DTSMSHRsrc   
    override type RsrcMsgTypes = DTSMSHRsrc   

    val protoDreqUUID = getUUID()
    val protoDrspUUID = getUUID()    
    
    lazy val aLabel = new CnxnCtxtLeaf[String,String,String]( Left( "a" ) )

    object MonadicDRsrcMsgs extends RsrcMsgTypes with Serializable {
      
      override def protoDreq : DReq = MDGetRequest( aLabel )
      override def protoDrsp : DRsp = MDGetResponse( aLabel, 0.0 )
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
    
    override def protoMsgs : MsgTypes = MonadicDRsrcMsgs
    override def protoRsrcMsgs : RsrcMsgTypes = MonadicDRsrcMsgs
  }
  
  object MolecularUseCase {
    import MonadicKVDBNet._
    import KVDBNodeFactory._

    implicit val retTwist : Boolean = false
    def setup(
      localHost : String, localPort : Int,
      remoteHost : String, remotePort : Int
    )(
      implicit returnTwist : Boolean
    ) : Either[MonadicKVDBNode,(MonadicKVDBNode,MonadicKVDBNode)] = {
      val ( localExchange, remoteExchange ) = 
	if ( localHost.equals( remoteHost ) && ( localPort == remotePort ) ) {
	  ( "/molecularUseCaseProtocolLocal", "/molecularUseCaseProtocolRemote" )	  
	}
	else {
	  ( "/molecularUseCaseProtocol", "/molecularUseCaseProtocol" )	  
	}

      if ( returnTwist ) {
	Right[MonadicKVDBNode,(MonadicKVDBNode,MonadicKVDBNode)](
	  (
	    ptToPt(
	      new URI( "agent", null, localHost, localPort, localExchange, null, null ),
	      new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
	    ),
	    ptToPt(	      
	      new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null ),
	      new URI( "agent", null, localHost, localPort, localExchange, null, null )
	    )
	  )
	)
      }
      else {
	Left[MonadicKVDBNode,(MonadicKVDBNode,MonadicKVDBNode)](
	  ptToPt(
	    new URI( "agent", null, localHost, localPort, localExchange, null, null ),
	    new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
	  )
	)
      }
    }

    object KinaseSpecifications {
      import scala.math._
      
      trait Kinase {
	def b : Boolean
	def i : Int
	def state : String
      }
      case class RAF(
	b : Boolean, i : Int, state : String
      ) extends Kinase
      case class RAS(
	b : Boolean, i : Int, state : String
      ) extends Kinase
      case class MEK1(
	b : Boolean, i : Int, state : String
      ) extends Kinase
      case class MEK2(
	b : Boolean, i : Int, state : String
      ) extends Kinase
      case class MAPK(
	b : Boolean, i : Int, state : String
      ) extends Kinase    
      
      lazy val RAFProto : RAF = RAF( true, 0, "Phosphorylated" )
      lazy val RASProto : RAS = RAS( true, 0, "Phosphorylated" )
      lazy val MEK1Proto : MEK1 = MEK1( true, 0, "Phosphorylated" )
      lazy val MEK2Proto : MEK2 = MEK2( true, 0, "Phosphorylated" )      
      lazy val MAPKProto : MAPK = MAPK( true, 0, "Phosphorylated" )      

      def mkMolQry( kinase : Kinase ) : CnxnCtxtLabel[String,String,String] = {
	import CnxnConversionStringScope._
	kinase match {
	  case cc : ScalaObject with Product with Serializable => {
	    asCnxnCtxtLabel( cc )
	  }
	  case _ => throw new Exception( "non concrete kinase: " + kinase )
	}	
      }

      def mkMolPtn( molType : String ) : CnxnCtxtLabel[String,String,String] = {
	new CnxnCtxtBranch[String,String,String](
	  "comBiosimilarityLiftModelStoreUsageMolecularUseCase_KinaseSpecifications_" + molType,
	  List( 
	    new CnxnCtxtLeaf[String,String,String](
	      Right[String,String]( "B" )
	    ),
	    new CnxnCtxtLeaf[String,String,String](
	      Right[String,String]( "I" )
	    ),
	    new CnxnCtxtBranch[String,String,String](
	      "state",
	      List(
		new CnxnCtxtLeaf[String,String,String](
		  Left[String,String]( "Phosphorylated" )
		)
	      )
	    )
	  )
	)
      }

      type ConcreteKinase = Kinase with Product with Serializable

      lazy val molPtnMap : HashMap[ConcreteKinase,CnxnCtxtLabel[String,String,String]] = {	
	val map = new HashMap[ConcreteKinase,CnxnCtxtLabel[String,String,String]]()
	map += ( RAFProto -> mkMolPtn( "RAF" ) )
	map += ( RASProto -> mkMolPtn( "RAS" ) )
	map += ( MEK1Proto -> mkMolPtn( "MEK1" ) )
	map += ( MEK2Proto -> mkMolPtn( "MEK2" ) )
	map += ( MAPKProto -> mkMolPtn( "MAPK" ) )
	map
      }

      implicit lazy val cascade : Seq[ConcreteKinase] =
	List[ConcreteKinase](
	  RAFProto, RASProto, MEK1Proto, MEK2Proto, MAPKProto
	)

      implicit lazy val cascadeInitialState : List[( ConcreteKinase, Option[ConcreteKinase] )] = {
	cascade.zip( cascade.drop( 1 ).map( Some( _ ) ) ++ List( None ) ).toList
      }

      implicit lazy val initialKinaseToProduce : ConcreteKinase = {	
	cascade.head
      }	

      def raf2RAS : Double = random * 100
      def ras2MEK1 : Double = random * 100
      def mek12MEK2 : Double = random * 100
      def mek22MAPK : Double = random * 100
      def mapk2Protein : Double = random * 100            

      lazy val cascadeTransitionMap : HashMap[( ConcreteKinase,	Option[ConcreteKinase] ),Double] = {
	val map = new HashMap[( ConcreteKinase,	Option[ConcreteKinase] ),Double]()
	map += ( ( RAFProto, Some( RASProto ) ) -> raf2RAS )
	map += ( ( RAFProto, None ) -> raf2RAS ) // Assume transition to RAS
	map += ( ( RASProto, Some( MEK1Proto ) ) -> ras2MEK1 )
	map += ( ( RASProto, None ) -> ras2MEK1 ) // Assume transition to MEK1
	map += ( ( MEK1Proto, Some( MEK2Proto ) ) -> mek12MEK2 )
	map += ( ( MEK1Proto, None ) -> mek12MEK2 ) // Assume transition to MEK2
	map += ( ( MAPKProto, None ) -> mapk2Protein )
	map
      }

      lazy val RAFPtn : CnxnCtxtLabel[String,String,String] =
	molPtnMap( RAFProto )
      
      lazy val RASPtn : CnxnCtxtLabel[String,String,String] =
	molPtnMap( RASProto )

      lazy val MEK1Ptn : CnxnCtxtLabel[String,String,String] =
	molPtnMap( MEK1Proto )

      lazy val MEK2Ptn : CnxnCtxtLabel[String,String,String] =
	molPtnMap( MEK2Proto )
            
      lazy val MAPKPtn : CnxnCtxtLabel[String,String,String] =
	molPtnMap( MAPKProto )
    }

    import KinaseSpecifications._

    trait CellularEnvironment {
      def kinaseMap : HashMap[Kinase,Double] 
      def amt [K <: Kinase] ( proto : K ) : Double = {
	kinaseMap.get( proto ).getOrElse( 0 )
      }      
    }
    
    case class Cytoplasm( kinaseMap : HashMap[Kinase,Double] )
	 extends CellularEnvironment with MapProxy[Kinase,Double] {
	   override def self = kinaseMap
	 }

    implicit lazy val cellCytoplasm : Cytoplasm = Cytoplasm( new HashMap[Kinase,Double]() )    

    def supplyKinase(
      kvdbNode : MonadicKVDBNode,
      cellCytoplasm : Cytoplasm,
      kinase : ConcreteKinase,
      trigger : Double
    ) : Unit = {
      import scala.math._
      import CnxnConversionStringScope._
      import cnxnConversions._
      new Thread {
	override def run() : Unit = {
	  def loop( kinase : ConcreteKinase, amt : Double ) : Unit = {
	    val kamt = cellCytoplasm.amt( kinase )
	    if ( kamt < amt ) {
	      val inc = random * 25
	      cellCytoplasm += ( kinase -> ( kamt + inc ) )
	      reset { 
		println(
		  (
		    ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		    + kvdbNode + "releasing an increment " + inc + " of " + kinase + "\n"
		    + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
		  )
		)
		kvdbNode.put( mkMolQry( kinase ), inc )
	      }
	      loop( kinase, amt )
	    }
	  }

	  loop( kinase, trigger )

	}
      }.start
    }

    def supplyKinaseInc(
      kvdbNode : MonadicKVDBNode,
      cellCytoplasm : Cytoplasm,
      kinase : ConcreteKinase,
      trigger : Double
    ) : Unit = {
      import scala.math._
      import CnxnConversionStringScope._
      import cnxnConversions._
      new Thread {
	override def run() : Unit = {
	  val kamt = cellCytoplasm.amt( kinase )
	  if ( kamt < trigger ) {
	    val inc = random * 25
	    cellCytoplasm += ( kinase -> ( kamt + inc ) )
	    reset { 
	      println(
		(
		  ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		  + kvdbNode + "releasing an increment " + inc + " of " + kinase + "\n"
		  + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
		)
	      )
	      kvdbNode.put( mkMolQry( kinase ), inc )
	    }
	  }
	}
      }.start
    }

    def consumeKinase(
      kvdbNode : MonadicKVDBNode,
      cellCytoplasm : Cytoplasm,
      previous : Option[( ConcreteKinase, Option[ConcreteKinase] )]
    )(
      implicit cascadeState : List[( ConcreteKinase, Option[ConcreteKinase] )]
    ) : Unit = {
      if ( !cascadeState.isEmpty ) {

	val state@( kinaseToConsumeProto, optKinaseToProduceProto ) = cascadeState.head
	val kinasePtn = molPtnMap( kinaseToConsumeProto )
	val trigger = cascadeTransitionMap.get( state ).getOrElse( java.lang.Double.MAX_VALUE )

	reset {
	  // Wait for kinase
	  for( kinaseRsrc <- kvdbNode.get( kinasePtn ) ) {
	    println(
	      (
		">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		+ kvdbNode + " received resource : " + kinaseRsrc + "\n"
		+ ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
	      )
	    )
	    kinaseRsrc match {
	      // Got some!
	      case Some( mTT.RBoundAList( Some( mTT.Ground( inc ) ), soln ) ) => {
		println(
		  (
		    ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		    + kvdbNode
		    + " received an increment, "
		    + inc
		    + ", of "
		    + kinaseToConsumeProto + "\n"
		    + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
		  )
		)
		val currAmt : Double = cellCytoplasm.amt( kinaseToConsumeProto )
		cellCytoplasm += ( ( kinaseToConsumeProto, ( currAmt + inc ) ) )

		println(
		  (
		    ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		    + kvdbNode
		    + " has accumulated "
		    + currAmt + inc
		    + " of "
		    + kinaseToConsumeProto + "\n"
		    + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
		  )
		)

		optKinaseToProduceProto match {
		  case Some( kinaseToProduceProto ) => {
		    for( amt <- cellCytoplasm.get( kinaseToConsumeProto ) ) {
		      // Got enough!
		      if ( amt > trigger ) {		    		    
			println( 
			  (
			    ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
			    + kvdbNode
			    + " received enough "
			    + kinaseToConsumeProto
			    + " to produce "
			    + kinaseToProduceProto + "\n"
			    + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
			  )
			)
			
			val nextCascadeState = cascadeState.drop( 2 )
			
			for( nextTrigger <- cascadeTransitionMap.get( nextCascadeState.head ) ) {
			  // Supply some RAS
			  supplyKinaseInc(
			    kvdbNode,
			    cellCytoplasm,
			    kinaseToProduceProto,
			    nextTrigger
			  )
			  
			  // Begin waiting for MEK1
			  consumeKinase(
			    kvdbNode,
			    cellCytoplasm,
			    Some( state )
			  )(
			    nextCascadeState
			  )
			}
		      }
		      // Not quite enough...
		      else {
			println(
			  (
			    ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
			    + kvdbNode
			    + " still waiting for enough "
			    + kinaseToConsumeProto
			    + " to produce "
			    + kinaseToProduceProto + "\n"
			    + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
			  )
			)
			consumeKinase(
			  kvdbNode,
			  cellCytoplasm,
			  previous
			)(
			  cascadeState
			)
		      }
		    }		    		    
		  }
		  case _ => {
		    println( 
		      (
			">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
			+ kvdbNode + " producing Protein.\n"
			+ ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
		      )
		    )
		  }
		}		
	      }
	      // Got none... so wait
	      case None => {
		previous match {
		  case Some( s@( pktp, poktc ) ) => {
		    supplyKinaseInc(
		      kvdbNode,
		      cellCytoplasm,
		      pktp,
		      cascadeTransitionMap.get(	s ).getOrElse( java.lang.Double.MAX_VALUE )
		    )
		  }
		  case None => {
		    println( 
		      (
			">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
			+ kvdbNode + " received nothing; waiting for kinase, "
			+ kinaseToConsumeProto + ".\n"
			+ ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
		      )
		    )
		  }
		}
	      }
	      case unExpected@_ => {
		throw new Exception( "Protocol violated. Received: " + unExpected )
	      }	    
	    }
	  }
	}
      }
    }

    def runClient( kvdbNode : MonadicKVDBNode )( implicit cellCytoplasm : Cytoplasm ) : Unit = {
      import scala.math._
      import KinaseSpecifications._
      // map-reduce-style protocol checking      
      new Thread {
	//override def run() : Unit = rafLoop()
	override def run() : Unit = consumeKinase( kvdbNode, cellCytoplasm, None )
      }.start
    }

    def runServer( kvdbNode : MonadicKVDBNode )( implicit cellCytoplasm : Cytoplasm ) : Unit = {
      import scala.math._
      import KinaseSpecifications._
      // map-reduce-style protocol             
      new Thread {
	override def run() : Unit = {
	  supplyKinaseInc( kvdbNode, cellCytoplasm, RAFProto, raf2RAS )
	  //rasLoop()
	  consumeKinase(
	    kvdbNode, cellCytoplasm, Some( cascadeInitialState.head )
	  )( cascadeInitialState.drop( 1 ) )
	}
      }.start
    }
    
  }
}
