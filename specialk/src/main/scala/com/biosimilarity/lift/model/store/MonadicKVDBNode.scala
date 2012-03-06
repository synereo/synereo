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
      val xstrm = new XStream( new JettisonMappedXmlDriver )
      val fmsg = xstrm.fromXML( txPortMsg ).asInstanceOf[A]
      tweet( "fmsg : " + fmsg )
      fmsg 
    }
    override def framedMsg2TxPort [A >: FramedMsg] ( txPortMsg : A ) : String = {
      val xstrm = new XStream( new JettisonMappedXmlDriver )
      val xmsg = xstrm.toXML( txPortMsg )
      tweet( "txPortMsg : " + xmsg )
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

    def wrapResponse(
      msrc : Moniker, dreq : Msgs.DReq, rsrc : mTT.Resource
    ) : FramedMsg = {
      frameResponse( msrc )(
	dreq match {
	  case Msgs.MDGetRequest( path ) => {
	    RsrcMsgs.MDGetResponseRsrc[Namespace,Var,Tag,Value]( path, rsrc )	  
	  }
	  case Msgs.MDFetchRequest( path ) => {
	    RsrcMsgs.MDFetchResponseRsrc[Namespace,Var,Tag,Value]( path, rsrc )
	  }
	  case Msgs.MDSubscribeRequest( path ) => {
	    RsrcMsgs.MDSubscribeResponseRsrc[Namespace,Var,Tag,Value]( path, rsrc )
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
      val Left( JustifiedRequest( msgId, mtrgt, msrc, lbl, body, _ ) ) = dreq

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
      mget( dAT.AGetNum, hops )( theMeetingPlace, theWaiters, true, cursor )( path )    
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
	theMeetingPlace, theWaiters, false, cursor
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
	theChannels, theSubscriptions, true, false
      )( path )    
    }
    
    override def subscribe(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      subscribe( Nil )( path )    
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
      def raf2RAS : Double = random * 100
      def ras2MEK1 : Double = random * 100
      def mek12MEK2 : Double = random * 100
      def mek22MAPK : Double = random * 100
      def mapk2Protein : Double = random * 100

      trait Kinase
      case class RAF(
	b : Boolean, i : Int, a : String
      ) extends Kinase
      case class RAS(
	b : Boolean, i : Int, a : String
      ) extends Kinase
      case class MEK1(
	b : Boolean, i : Int, a : String
      ) extends Kinase
      case class MEK2(
	b : Boolean, i : Int, a : String
      ) extends Kinase
      case class MAPK(
	b : Boolean, i : Int, a : String
      ) extends Kinase    
      
      val RAFProto : RAF = RAF( true, 0, "Phosphorylated" )
      val RASProto : RAS = RAS( true, 0, "Phosphorylated" )
      val MEK1Proto : MEK1 = MEK1( true, 0, "Phosphorylated" )
      val MEK2Proto : MEK2 = MEK2( true, 0, "Phosphorylated" )      
      val MAPKProto : MAPK = MAPK( true, 0, "Phosphorylated" )

      def mkMolPtn( molType : String ) : CnxnCtxtLabel[String,String,String] = {
	new CnxnCtxtBranch[String,String,String](
	  "comBiosimilarityLiftModelStoreUsageMolecularUseCase_KinaseSpecifications_" + molType,
	  List( 
	    new CnxnCtxtLeaf[String,String,String](
	      Right[String,String]( "B" )
	    ),
	    new CnxnCtxtBranch[String,String,String](
	      "a",
	      List(
		new CnxnCtxtLeaf[String,String,String](
		  Left[String,String]( "Phosphorylated" )
		)
	      )
	    ),
	    new CnxnCtxtLeaf[String,String,String](
	      Right[String,String]( "I" )
	    )
	  )
	)
      }

      val RASPtn : CnxnCtxtLabel[String,String,String] =
	mkMolPtn( "RAS" )

      val RAFPtn : CnxnCtxtLabel[String,String,String] =
	mkMolPtn( "RAF" )
      
      val MEK1Ptn : CnxnCtxtLabel[String,String,String] =
	mkMolPtn( "MEK1" )

      val MEK2Ptn : CnxnCtxtLabel[String,String,String] =
	mkMolPtn( "MEK2" )
            
      val MAPKPtn : CnxnCtxtLabel[String,String,String] =
	mkMolPtn( "MAPK" )
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
      kinase : Kinase with ScalaObject with Product with Serializable,
      trigger : Double
    ) : Unit = {
      import scala.math._
      import CnxnConversionStringScope._
      import cnxnConversions._
      new Thread {
	override def run() : Unit = {
	  def loop( kinase : Kinase with ScalaObject with Product with Serializable, amt : Double ) : Unit = {
	    val kamt = cellCytoplasm.amt( kinase )
	    if ( kamt < amt ) {
	      val inc = random * 25
	      cellCytoplasm += ( kinase -> ( kamt + inc ) )
	      reset { kvdbNode.put( asCnxnCtxtLabel( kinase ), inc ) }
	      loop( kinase, amt )
	    }
	  }

	  loop( kinase, trigger )

	}
      }.start
    }

    def runClient( kvdbNode : MonadicKVDBNode )( implicit cellCytoplasm : Cytoplasm ) : Unit = {
      import scala.math._
      import KinaseSpecifications._
      // map-reduce-style protocol       

      supplyKinase( kvdbNode, cellCytoplasm, RAFProto, raf2RAS )

      new Thread {
	override def run() : Unit = {
	  reset {
	    for( rasRsrc <- kvdbNode.get( RASPtn ) ) {
	      println( kvdbNode + " received: " + rasRsrc )
	      rasRsrc match {
		case Some( mTT.RBound( Some( mTT.Ground( inc ) ), soln ) ) => {
		  println( kvdbNode + " received an increment, " + inc + ", of RAS" )
		  val currAmt : Double = cellCytoplasm.amt( RASProto )
		  cellCytoplasm += ( ( RASProto, ( inc + currAmt ) ) )
		  for( amt <- cellCytoplasm.get( RASProto ) ) {
		    if ( amt > ras2MEK1 ) {
		      supplyKinase( kvdbNode, cellCytoplasm, MEK1Proto, mek12MEK2 )
		      reset {
			for( mek2Rsrc <- kvdbNode.get( MEK2Ptn ) ) {
			  println( kvdbNode + " received: " + mek2Rsrc )
			  mek2Rsrc match {
			    case Some( mTT.RBound( Some( mTT.Ground( inc ) ), soln ) ) => {
			      println( kvdbNode + " received an increment, " + inc + ", of MEK2" )
			      val currAmt : Double = cellCytoplasm.amt( MEK2Proto )
			      cellCytoplasm += ( ( MEK2Proto, ( currAmt + inc ) ) )
			      for( amt <- cellCytoplasm.get( MEK2Proto ) ) {
				if ( amt > mek22MAPK ) {
				  supplyKinase( kvdbNode, cellCytoplasm, MAPKProto, mapk2Protein )
				  println( "MAPK produced." )
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
		}
		case None => {
		  println( "Waiting is, Water Brother." )
		}
		case unExpected@_ => {
		  throw new Exception( "Protocol violated. Received: " + unExpected )
		}	    
	      }
	    }
	  }
	}
      }.start
    }

    def runServer( kvdbNode : MonadicKVDBNode )( implicit cellCytoplasm : Cytoplasm ) : Unit = {
      import scala.math._
      import KinaseSpecifications._
      // map-reduce-style protocol checking                        

      new Thread {
	override def run() : Unit = {
	  reset {
	    for( rafRsrc <- kvdbNode.get( RAFPtn ) ) {
	      println( kvdbNode + " received: " + rafRsrc )
	      rafRsrc match {
		case Some( mTT.RBound( Some( mTT.Ground( inc ) ), soln ) ) => {
		  println( kvdbNode + " received an increment, " + inc + ", of RAF" )
		  val currAmt : Double = cellCytoplasm.amt( RAFProto )
		  cellCytoplasm += ( ( RAFProto, ( currAmt + inc ) ) )
		  for( amt <- cellCytoplasm.get( RAFProto ) ) {
		    if ( amt > raf2RAS ) {
		      supplyKinase( kvdbNode, cellCytoplasm, RASProto, ras2MEK1 )
		      reset {
			for( mek1Rsrc <- kvdbNode.get( MEK1Ptn ) ) {
			  println( kvdbNode + " received: " + mek1Rsrc )
			  mek1Rsrc match {
			    case Some( mTT.RBound( Some( mTT.Ground( inc ) ), soln ) ) => {
			      println( kvdbNode + " received an increment, " + inc + ", of MEK1" )
			      val currAmt : Double = cellCytoplasm.amt( MEK1Proto )
			      cellCytoplasm += ( ( MEK1Proto, ( currAmt + inc ) ) )
			      for( amt <- cellCytoplasm.get( MEK1Proto ) ) {
				if ( amt > mek12MEK2 ) {
				  supplyKinase( kvdbNode, cellCytoplasm, MEK2Proto, mek22MAPK )
				  reset {
				    for( mapkRsrc <- kvdbNode.get( MAPKPtn ) ) {
				      println( kvdbNode + " received: " + mapkRsrc )
				      mapkRsrc match {
					case Some( mTT.RBound( Some( mTT.Ground( inc ) ), soln ) ) => {
					  println( kvdbNode + " received an increment, " + inc + ", of MAPK" )
					  val currAmt : Double = cellCytoplasm.amt( MAPKProto )
					  cellCytoplasm += ( ( MAPKProto, ( currAmt + inc ) ) )
					  for( amt <- cellCytoplasm.get( MAPKProto ) ) {
					    if ( amt > mapk2Protein ) {
					      println( "Protein produced." )
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
			    }
			    case unExpected@_ => {
			      throw new Exception( "Protocol violated. Received: " + unExpected )
			    }
			  }
			}
		      }
		    }
		  }
		}		
		case None => {
		  println( "Waiting is, Water Brother." )
		}
		case unExpected@_ => {
		  throw new Exception( "Protocol violated. Received: " + unExpected )
		}	    
	      }
	    }
	  }
	}
      }.start
    }
    
  }
}
