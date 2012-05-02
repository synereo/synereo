// -*- mode: Scala;-*- 
// Filename:    AgentKVDBNode.scala 
// Authors:     lgm                                                    
// Creation:    Mon Apr 30 20:37:26 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra.agentservicesstore

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
import java.io.File
import java.io.FileInputStream
import java.io.OutputStreamWriter
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayOutputStream

trait AgentKVDBNodeScope[Namespace,Var,Tag,Value] 
extends PersistedMonadicKVDBNodeScope[Namespace,Var,Tag,Value]
with CnxnDTSMsgScope[Namespace,Var,Tag,Value]
with AgentCnxnTypeScope {
  trait AgentPersistenceScope extends PersistenceScope {
    class BaseAgentKVDB[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse, +KVDBNode[Rq <: ReqBody, Rs <: RspBody] <: BaseAgentKVDBNode[Rq,Rs,KVDBNode]](
      override val name : Moniker      
    ) extends BasePersistedMonadicKVDB[ReqBody,RspBody,KVDBNode](
      name
    ) {
    }

    object BaseAgentKVDB {
      def apply [ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse,KVDBNode[Rq <: ReqBody, Rs <: RspBody] <: BaseAgentKVDBNode[Rq,Rs,KVDBNode]] ( 
	name : Moniker
      ) : BaseAgentKVDB[ReqBody,RspBody,KVDBNode] = {
	new BaseAgentKVDB[ReqBody,RspBody,KVDBNode]( name )
      }
      def unapply [ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse,KVDBNode[Rq <: ReqBody, Rs <: RspBody] <: BaseAgentKVDBNode[Rq,Rs,KVDBNode]] (
	pmkvdb : BaseAgentKVDB[ReqBody,RspBody,KVDBNode]
      ) : Option[( Moniker )] = {
	Some( ( pmkvdb.name ) )
      }
    }

    class BaseAgentKVDBNode[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse,+KVDBNode[Rq <: ReqBody, Rs <: RspBody] <: BaseAgentKVDBNode[Rq,Rs,KVDBNode]](
      override val cache : BaseAgentKVDB[ReqBody,RspBody,KVDBNode],
      override val acquaintances : List[Moniker],
      val cnxn : Option[acT.AgentCnxn],
      val cnxnPartition : List[( acT.AgentCnxn, KVDBNode[ReqBody,RspBody] )]
    ) extends BasePersistedMonadicKVDBNode[ReqBody,RspBody,KVDBNode](
      cache, acquaintances
    ) with PersistenceManifestTrampoline
    with XMLIfy[Namespace,Var] {
      import identityConversions._
      def makeSpace( cnxn : acT.AgentCnxn ) : KVDBNode[ReqBody,RspBody] = {
	throw new Exception( "makeSpace is not defined on " + this )
      }
      def ptnCnxnWrapperNamespace : String = "patternConnection"
      def embedCnxn(
	cnxn : acT.AgentCnxn,
	ptn : CnxnCtxtLabel[Namespace,Var,Tag] with Factual
      ) : Option[CnxnCtxtLabel[Namespace,Var,Tag] with Factual] = {
	for(
	  ltns <- labelToNS;
	  ttt <- textToTag;
	  pm <- persistenceManifest;
	  if ( pm.isInstanceOf[XMLDBManifest] )
	) yield {
	  val xmldbPm = pm.asInstanceOf[XMLDBManifest]
	  val xmlifier = xmldbPm.xmlIfier
	  
	  val embeddedCnxn =
	    new CnxnCtxtBranch[Namespace,Var,Tag](
	      ltns( ptnCnxnWrapperNamespace ),
	      List(
		xmlIfier.tolabeledBlob [Namespace,Var,Tag]( ltns, ttt )( cnxn ),
		ptn
	      )
	    )
	  //	tweet(
	  //	  (
	  //	    ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
	  //	    + " embedding cnxn " + embeddedCnxn.toString
	  //	    + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
	  //	  )
	  //	)
	  embeddedCnxn
	}
      }
      
      def extractCnxn(
	ccl : CnxnCtxtLabel[Namespace,Var,Tag]
      ) : Option[( acT.AgentCnxn, CnxnCtxtLabel[Namespace,Var,Tag] with Factual )] = {
	ccl match {
	  case CnxnCtxtBranch( ptnCnxnNS, cnxnBlob :: rs :: Nil ) => {
	    for(
	      pm <- persistenceManifest;
	      if ( pm.isInstanceOf[XMLDBManifest] )	      
	    ) yield {
	      //	    tweet(
	      //	      (
	      //		">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
	      //		+ " embedded cnxn " + ccl.toString
	      //		+ ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
	      //	      )
	      //	    )
	      val xmldbPm = pm.asInstanceOf[XMLDBManifest]
	      val xmlifier = xmldbPm.xmlIfier
	      val prod =
		xmlifier.fromlabeledBlob( cnxnBlob ).getOrElse(
		  throw new Exception( "must have a cnxn" )
		)
	      ( prod.asInstanceOf[acT.AgentCnxn], rs )
	    }
	  }
	  case _ => {
	    None
	  }
	}
      }
      
      def forward( cnxn : acT.AgentCnxn )(
	ask : dAT.AskNum,
	hops : List[Moniker],
	path : CnxnCtxtLabel[Namespace,Var,Tag]
      ) : Unit = {
	
	tweet(
	  ( this + " in forwardGet with hops: " + hops )
	)
	
	for( trgt <- acquaintances; q <- stblQMap.get( trgt ) if !hops.contains( trgt ) ) {
	  tweet(
	    ( this + " forwarding to " + trgt )
	  )
	  // BUGBUG -- LGM: fix typing so we don't have to cast
	  for(
	    embeddedCnxn
	    <- embedCnxn(
	      cnxn,
	      path.asInstanceOf[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
	    )
	  ) {
	    val request = 
	      ask match {
		case dAT.AGetNum => {
		  Msgs.MDGetRequest[Namespace,Var,Tag,Value](
		    embeddedCnxn
		  ).asInstanceOf[Msgs.DReq]
		}
		case dAT.AFetchNum => {
		  Msgs.MDFetchRequest[Namespace,Var,Tag,Value](
		    embeddedCnxn
		  ).asInstanceOf[Msgs.DReq]
		}
		case dAT.ASubscribeNum => {
		  Msgs.MDSubscribeRequest[Namespace,Var,Tag,Value](
		    embeddedCnxn
		  ).asInstanceOf[Msgs.DReq]
		}
		case _ => throw new Exception( "askType not handled" )
	      }

	    request match {
	      case rqbdy : ReqBody => {
		val framedReq = frameRequest( trgt )( rqbdy )
		tweet( ( this + " forwarding " + framedReq + " to " + trgt ) )
		q ! framedReq
	      }
	      case _ => {
		throw new Exception( "unable to frame request: " + request )
	      }
	    }
	  }
	}
      }
    }

    object BaseAgentKVDBNode {
      def apply [ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse,KVDBNode[Rq <: ReqBody, Rs <: RspBody] <: BaseAgentKVDBNode[Rq,Rs,KVDBNode]] ( 
	cache : BaseAgentKVDB[ReqBody,RspBody,KVDBNode],
	acquaintances : List[Moniker],
	cnxn : Option[acT.AgentCnxn],
	cnxnPartition : List[( acT.AgentCnxn, KVDBNode[ReqBody,RspBody] )]
      ) : BaseAgentKVDBNode[ReqBody,RspBody,KVDBNode] = {
	new BaseAgentKVDBNode[ReqBody,RspBody,KVDBNode]( cache, acquaintances, cnxn, cnxnPartition )
      }
      def unapply [ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse,KVDBNode[Rq <: ReqBody, Rs <: RspBody] <: BaseAgentKVDBNode[Rq,Rs,KVDBNode]] (
	pmkvdbnode : BaseAgentKVDBNode[ReqBody,RspBody,KVDBNode]
      ) : Option[( BaseAgentKVDB[ReqBody,RspBody,KVDBNode], List[Moniker], Option[acT.AgentCnxn], List[( acT.AgentCnxn,KVDBNode[ReqBody,RspBody] )] )] = {
	Some( ( pmkvdbnode.cache, pmkvdbnode.acquaintances, pmkvdbnode.cnxn, pmkvdbnode.cnxnPartition ) )
      }
    }
  }
}
