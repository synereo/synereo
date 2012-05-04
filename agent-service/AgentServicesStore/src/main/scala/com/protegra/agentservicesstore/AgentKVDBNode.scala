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
      def mget( cnxn : acT.AgentCnxn )(
	persist : Option[PersistenceManifest],
	ask : dAT.AskNum,
	hops : List[Moniker]
      )(
	channels : Map[mTT.GetRequest,mTT.Resource],
	registered : Map[mTT.GetRequest,List[RK]],
	consume : RetentionPolicy,
	keep : RetentionPolicy,
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
                  val results = mget( channels, registered, consume, keep )( path )
                  var processed = false
		  
                  for(
                    oV <-results
                  ) {
                    oV match {
                      case None => {
                        persist match {
                          case None => {
                            rk(oV)
                          }
                          case Some(pd) => {
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
			    
                            checkIfDBExists(xmlCollName, true) match {
                              case true => {
                                val oQry = query(xmlCollName, path)
				
                                oQry match {
                                  case None => {
                                    rk(oV)
                                  }
                                  case Some(qry) => {
                                    tweet(
                                      (
                                        "querying db : " + pd.db
                                        + " from coll " + xmlCollName
                                        + " where " + qry
					
                                      )
                                    )
				    
                                    val rslts = executeWithResults(qry)
				    
                                    rslts match {
                                      case Nil => {
                                        tweet(
                                          (
                                            "database "
                                            + xmlCollName
                                            + " had no matching resources."
                                          )
                                        )
                                        rk(oV)
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
					
                                        // BUGBUG -- LGM : This is a
                                        // window of possible
                                        // failure; if we crash here,
                                        // then the result is out of
                                        // the store, but we haven't
                                        // completed processing. This is
                                        // where we need Tx.
					
                                        if ( cursor ) {
                                          var rsrcRslts: List[ mTT.Resource ] = Nil
					  
                                          for ( rslt <- itergen[ Elem ](rslts) ) {
                                            tweet("retrieved " + rslt.toString)
					    
                                             consume match {
					       case policy : RetainInStore => {
						 tweet("removing from store " + rslt)
						 removeFromStore(
                                                   persist,
                                                   rslt,
                                                   collName
						 )
					       }
					       case _ => {
						 tweet( "policy indicates not to remove from store " + rslt )
					       }
                                            }
					    
                                            val ersrc: emT.PlaceInstance = pd.asResource(path, rslt)
                                            ersrc.stuff match {
                                              case Left( r ) => rsrcRslts = r :: rsrcRslts
                                              case _ => {}
                                            }
                                          }
                                          val rsrcCursor = asCursor(rsrcRslts)
                                          //tweet( "returning cursor" + rsrcCursor )
                                          rk(rsrcCursor)
                                        }
                                            else {
                                              for ( rslt <- itergen[ Elem ](rslts) ) {
						
						tweet("retrieved " + rslt.toString)
						
						 consume match {
						   case policy : RetainInStore => {
						     tweet("removing from store " + rslt)
						     removeFromStore(
                                                       persist,
                                                       rslt,
                                                       collName
						     )
						   }
						   case _ => {
						     tweet( "policy indicates not to remove from store" + rslt )
						   }
						}
						
						val ersrc = pd.asResource(path, rslt)
						tweet("returning " + ersrc)						
						ersrc.stuff match {
						  case Left( r ) => rk( Some( r ) )
						  case _ => {}
						}
                                              }
                                            }
					
                                      }
                                    }
                                  }
                                }
                              }
                              case false => {
                                rk(oV)
                              }
                            }
                          }
                        }
                      }
                      case _ if ( !cursor )=> {
                        rk(oV)
                      }
                      case _ if ( cursor && !processed ) => {
                        var rsrcRslts: List[ mTT.Resource ] = Nil
                        for ( rslt <- results ) {
                          tweet("retrieved " + rslt.toString)
			  
                          rslt match {
                            case Some(r) => rsrcRslts = r :: rsrcRslts
                            case _ => {}
                          }
                        }
                        val rsrcCursor = asCursor(rsrcRslts)
                        //tweet( "returning cursor" + rsrcCursor )
                        processed = true
                        rk(rsrcCursor)
                      }
                      case _ => {
                        //cursor and processed, do nothing
                      }
                    }
                  }
		}
            }
	}
      }
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
      val partitionMap : List[( acT.AgentCnxn, KVDBNode[ReqBody,RspBody] )]
    ) extends BasePersistedMonadicKVDBNode[ReqBody,RspBody,KVDBNode](
      cache, acquaintances
    ) with PersistenceManifestTrampoline
    with XMLIfy[Namespace,Var] {
      import identityConversions._

      case class HashAgentKVDBNode[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
	override val cache : BaseAgentKVDB[ReqBody,RspBody,HashAgentKVDBNode],
	override val acquaintances : List[Moniker],
	override val cnxn : Option[acT.AgentCnxn]
      ) extends BaseAgentKVDBNode[ReqBody,RspBody,HashAgentKVDBNode](
	cache, acquaintances, cnxn, Nil
      )

      @transient
      var _cnxnPartition : Option[HashMap[acT.AgentCnxn,HashAgentKVDBNode[ReqBody,RspBody]]] = None
      def cnxnPartition : HashMap[acT.AgentCnxn,HashAgentKVDBNode[ReqBody,RspBody]] = {
	_cnxnPartition match { 
	  case Some( pm ) => pm
	  case None => {
	    val pm = new HashMap[acT.AgentCnxn,HashAgentKVDBNode[ReqBody,RspBody]]( )
	    for( ( cnxn, node ) <- partitionMap ) {
	      node match {
		case hakvdbn : HashAgentKVDBNode[ReqBody,RspBody] => {
		  pm += ( cnxn -> hakvdbn )
		}
		case _ => {
		  tweet( "warning: not hashing " + ( cnxn, node ) + " because node is not a compatible type" )
		}
	      }
	    }
	    _cnxnPartition = Some( pm )
	    pm
	  }
	}
      }

      def makeSpace( cnxn : acT.AgentCnxn ) : HashAgentKVDBNode[ReqBody,RspBody] = {
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
      
      def cnxnMatch(
	cnxn1 : acT.AgentCnxn,
	cnxn2 : acT.AgentCnxn
      ) : Boolean = {
	
	tweet(
	  (
	    "matching " + cnxn1 + " with " + cnxn2 + "\n"
	    + "cnxn1.src = " + cnxn1.src
	    + " cnxn2.src = " + cnxn2.src + "\n"
	    + " src's "
	    + (
	      if ( cnxn1.src == cnxn2.src ) {
		" match "
	      }
	      else {
		" do not match "
	      }
	    )
	    + "\n"
	    + "cnxn1.trgt = " + cnxn1.trgt
	    + " cnxn2.trgt = " + cnxn2.trgt + "\n"
	    + " trgt's "
	    + (
	      if ( cnxn1.trgt == cnxn2.trgt ) {
		" match "
	      }
	      else {
		" do not match "
	      }
	    )
	    + "\n"
	    + "cnxn1.label = " + cnxn1.label
	    + " cnxn2.label = " + cnxn2.label + "\n"
	    + " label's "
	    + (
	      if ( cnxn1.label == cnxn2.label ) {
		" match "
	      }
	      else {
		" do not match "
	      }
	    )
	  )
          
	)
	
	val rslt =
	  (( cnxn1 == cnxn2 )
	   || (( cnxn1.src == cnxn2.src )
	       && ( cnxn1.trgt == cnxn2.trgt )
	       && ( cnxn1.label == cnxn2.label)));
	
	tweet( if ( rslt ) "matched" else "did not match" )
	
	rslt
      }    
      
      def searchCnxnPartition(
	cnxn : acT.AgentCnxn
      ) : Option[HashAgentKVDBNode[ReqBody,RspBody]] = {
	// local recursion that stops just when the match is found
	def search(
	  pairs : List[( acT.AgentCnxn, HashAgentKVDBNode[ReqBody,RspBody] )]
	) : Option[( acT.AgentCnxn, HashAgentKVDBNode[ReqBody,RspBody] )] = {
	  pairs match {
	    case ( cnxnKey, part ) :: rpairs => {
	      if ( cnxnMatch( cnxn, cnxnKey ) ) {
		Some( ( cnxnKey, part ) )
	      }
	      else {
		search( rpairs )
	      }
	    }
	    case Nil => None
	  }
	}
	
	tweet(
	  "Map failed searching map pairs : " + cnxnPartition.toList
	)
	
	for( ( cnxnKey, part ) <- search( cnxnPartition.toList ) ) 
	yield { part }       
      }
      
      def getPartition(
	cnxn : acT.AgentCnxn
      ) : HashAgentKVDBNode[ReqBody,RspBody] = {
	cnxnPartition.get( cnxn ) match {
	  // BUGBUG -- LGM : this is a workaround until we have
	  // invitation and introduction protocols for PlatformAgents
	  case None => {
	    searchCnxnPartition( cnxn ) match {
	      case None => {
		tweet(
		  (
		    "No matching space for "
		    + cnxn
		    + "\n"
		    + "Creating a new one"
		  )
		)
		val npmgj = makeSpace( cnxn )
		cnxnPartition( cnxn ) = npmgj
		npmgj
	      }
	      case Some( npmgj ) => {
		tweet(
		  "Found cnxn matching through search " + cnxn + "\n"
		)
		npmgj
	      }
	    }
	  }
	  case Some( npmgj ) => {
	    tweet(
	      "Found matching space for " + cnxn
	    )
	    npmgj
	  }
	}
      }
      
      def getLocalPartition(
	cnxn : acT.AgentCnxn   // C( localProvider, l, remoteRequester )
      ) : HashAgentKVDBNode[ReqBody,RspBody] = {
	tweet(
	  "Getting local partition using " + cnxn
	)
	getPartition( cnxn )   // C( localProvider, l, remoteRequester )
      }
      
      def getRemotePartition(
	cnxn : acT.AgentCnxn   // C( remoteRequester, l, localProvider )
      ) : HashAgentKVDBNode[ReqBody,RspBody] = {
	tweet(
	  "Getting remote partition using " + cnxn
	)
	//       val rvrsCnxn =         // C( localProvider, l, remoteRequester )
	// 	acT.AgentCnxn( cnxn.trgt, cnxn.label, cnxn.src )
	
	//       getLocalPartition( rvrsCnxn )
	// Since there is now a queue/partition we could only have
	// received this on the partition handling this queue; hence,
	// there is no further need for lookup
	cnxnPartition.get( cnxn ) match {
	  case Some( rp ) => rp 
	  case None => {
	    val rp = new HashAgentKVDBNode[ReqBody,RspBody](
	      cache.asInstanceOf[BaseAgentKVDB[ReqBody,RspBody,HashAgentKVDBNode]],
	      acquaintances,
	      Some( cnxn )
	    )
	    cnxnPartition += ( cnxn -> rp )
	    rp
	  }
	}
      }
      
      def getPartitionActuals(
	cnxn : acT.AgentCnxn,
	partFn : acT.AgentCnxn => HashAgentKVDBNode[ReqBody,RspBody]
      ) = {
	val pmgj : HashAgentKVDBNode[ReqBody,RspBody] = partFn( cnxn )
	val perD = pmgj.persistenceManifest
	val xmlCollName = 
	  perD match {
	    case None => None
	    case Some( pd ) => Some( pd.storeUnitStr( cnxn ) )
	  }
	( pmgj, perD, xmlCollName )
      }
      
      def getLocalPartitionActuals( cnxn : acT.AgentCnxn ) = {
	getPartitionActuals( cnxn, getLocalPartition )      
      }
      
      def getRemotePartitionActuals( cnxn : acT.AgentCnxn ) = {
	getPartitionActuals( cnxn, getRemotePartition ) 
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

      def mget( cnxn : acT.AgentCnxn )(
	persist : Option[PersistenceManifest],
	ask : dAT.AskNum,
	hops : List[Moniker]
      )(
	channels : Map[mTT.GetRequest,mTT.Resource],
	registered : Map[mTT.GetRequest,List[RK]],
	consume : RetentionPolicy,
	keep : RetentionPolicy,
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
		  tweet( 
		    (
		      "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		      + "mgetting " + path + ".\n"
		      + "on " + this + ".\n"
		      + "checking local cache " + cache + ".\n"
		      + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
		    )
		  )
		  for(
		    oV <- cache.mget( persist, ask, hops )( channels, registered, consume, keep, cursor, collName )( path ) 
		  ) {
		    oV match {
		      case None => {
			//tweet( ">>>>> forwarding..." )
			tweet( 
			  (
			    "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
			    + "mgetting " + path + ".\n"
			    + "on " + cache + " did not find a resource.\n"
			    + "xml collection name: " + collName + "\n"
			    + "consume data : " + true + "\n"
			    + "keep continuation : " + true + "\n"
			    + "forwarding to acquaintances.\n"
			    + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
			  )
			)
			
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
      
      def remotePut( cnxn : acT.AgentCnxn )(
	ptn : mTT.GetRequest, rsrc : mTT.Resource
      ) = {
	tweet(
	  "In cnxn-based put with cnxn " + cnxn
          
	)
	
	val ( pmgj, perD, xmlCollName ) =	getRemotePartitionActuals( cnxn )
	
	tweet(
	  "Storing " + ptn + " " + rsrc + " in partition " + pmgj
          
	)
	
	pmgj.cache.mput( perD )(
	  pmgj.cache.theMeetingPlace, pmgj.cache.theWaiters, false, xmlCollName
	)( ptn, rsrc )
      }
      
      def remotePublish( cnxn : acT.AgentCnxn )(
	ptn : mTT.GetRequest, rsrc : mTT.Resource
      ) = {
	tweet(
	  "In cnxn-based publish with cnxn " + cnxn
          
	)
	val ( pmgj, perD, xmlCollName ) = getRemotePartitionActuals( cnxn )
	
	tweet(
	  "Publishing " + rsrc + " on " + ptn + " in partition " + pmgj
          
	)
	
	pmgj.cache.mput( perD )(
	  pmgj.cache.theChannels, pmgj.cache.theSubscriptions, true, xmlCollName
	)( ptn, rsrc )
      }

      def remoteGet( hops : List[Moniker] )(
	cnxn : acT.AgentCnxn
      )(
	path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {
	tweet(
	  "In cnxn-based get with cnxn " + cnxn
          
	)
	
	val ( pmgj, perD, xmlCollName ) = getRemotePartitionActuals( cnxn )
	
	tweet(
	  "Retrieving " + path + " from partition " + pmgj
          
	)
	
	pmgj.mget( cnxn )( perD, dAT.AGetNum, hops )(
	  pmgj.theMeetingPlace, pmgj.theWaiters, CacheAndStore, Store, false, xmlCollName
	)( path ).asInstanceOf[Generator[Option[mTT.Resource],Unit,Unit]]
      }

      def remoteFetch( hops : List[Moniker] )(
	cnxn : acT.AgentCnxn
      )(
	path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {   
	tweet(
	  "In cnxn-based fetch with cnxn " + cnxn
          
	)
	
	val ( pmgj, perD, xmlCollName ) =	getRemotePartitionActuals( cnxn )
	
	tweet(
	  "Retrieving " + path + " from partition " + pmgj
          
	)
	
	pmgj.mget( cnxn )( perD, dAT.AFetchNum, hops )(
	  pmgj.theMeetingPlace, pmgj.theWaiters, DoNotRetain, DoNotRetain, false, xmlCollName
	)( path ).asInstanceOf[Generator[Option[mTT.Resource],Unit,Unit]]
      }

      def remoteSubscribe( hops : List[Moniker] )(
	cnxn : acT.AgentCnxn
      )(
	path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {    
	tweet(
	  "In cnxn-based subscribe with cnxn " + cnxn
          
	)
	
	val ( pmgj, perD, xmlCollName ) = getRemotePartitionActuals( cnxn )
	
	tweet(
	  "Retrieving " + path + " from partition " + pmgj
          
	)
	
	pmgj.mget( cnxn )( perD, dAT.ASubscribeNum, hops )(
	  pmgj.theChannels, pmgj.theSubscriptions, CacheAndStore, Store, false, xmlCollName
	)( path ).asInstanceOf[Generator[Option[mTT.Resource],Unit,Unit]]
      }            
      
      override def dispatchDMsg( dreq : FramedMsg ) : Unit = {
	dreq match {
	  case Left( JustifiedRequest( msgId, mtrgt, msrc, lbl, body, _ ) ) => {
	    body match {
	      case dgreq@Msgs.MDGetRequest( path ) => {	  
		for( ( cnxn, npath ) <- extractCnxn( path ) ) {
		  tweet( ( this + " getting locally for location : " + path ) )
		  reset {
		    for( v <- remoteGet( List( msrc ) )( cnxn )( npath ) ) {
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
	      }
	      
	      case dfreq@Msgs.MDFetchRequest( path ) => {
		for( ( cnxn, npath ) <- extractCnxn( path ) ) {
		  tweet( ( this + "fetching locally for location : " + path ) )
		  reset {
		    for( v <- remoteFetch( List( msrc ) )( cnxn )( path ) ) {
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
	      }
	      
	      case dsreq@Msgs.MDSubscribeRequest( path ) => {
		for( ( cnxn, npath ) <- extractCnxn( path ) ) {
		  tweet( ( this + "subscribing locally for location : " + path ) )
		  reset {
		    for( v <- remoteSubscribe( List( msrc ) )( cnxn )( npath ) ) {
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
	  // BUGBUG -- lgm : DRY this please
	  case Right( JustifiedResponse( msgId, mtrgt, msrc, lbl, body, _ ) ) => {
	    body match {
	      case RsrcMsgs.MDGetResponseRsrc( path, rsrc ) => {
		for( ( cnxn, npath ) <- extractCnxn( path ) ) {
		  rsrc match {
		    // if the rsrc comes with a substitution
		    // apply that to the path
		    case rbnd : mTT.RBound => {
		      rsrc( npath ) match {
			// if the application results in a specialization
			// put the results there
			case Some( spec ) => {
			  for( inrRsrc <- rbnd.rsrc ) {			  
			    reset { remotePut( cnxn )( spec, inrRsrc ) }
			  }		      		  
			}
			// else put the results at the path
			case None => {
			  reset { remotePut( cnxn )( npath, rsrc ) }
			}
		      }		  
		    }
		    case _ => {
		      reset { remotePut( cnxn )( npath, rsrc ) }
		    }
		  }
		}
	      }
	      case RsrcMsgs.MDFetchResponseRsrc( path, rsrc ) => {
		for( ( cnxn, npath ) <- extractCnxn( path ) ) {
		  rsrc match {
		    case rbnd : mTT.RBound => {
		      rsrc( npath ) match {
			case Some( spec ) => {
			  for( inrRsrc <- rbnd.rsrc ) {
			    reset { remotePut( cnxn )( spec, inrRsrc ) }
			  }		      		  
			}
			case None => {
			  reset { remotePut( cnxn )( npath, rsrc ) }
			}
		      }		  
		    }
		    case _ => {
		      reset { remotePut( cnxn )( npath, rsrc ) }
		    }
		  }
		}
	      }
	      case RsrcMsgs.MDSubscribeResponseRsrc( path, rsrc ) => {
		for( ( cnxn, npath ) <- extractCnxn( path ) ) {
		  rsrc match {
		    case rbnd : mTT.RBound => {
		      rsrc( npath ) match {
			case Some( spec ) => {
			  for( inrRsrc <- rbnd.rsrc ) {
			    reset { remotePublish( cnxn )( spec, inrRsrc ) }
			  }		      		  
			}
			case None => {
			  reset { remotePublish( cnxn )( npath, rsrc ) }
			}
		      }		  
		    }
		    case _ => {
		      reset { remotePublish( cnxn )( npath, rsrc ) }
		    }
		  }
		}
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
	Some( ( pmkvdbnode.cache, pmkvdbnode.acquaintances, pmkvdbnode.cnxn, pmkvdbnode.partitionMap ) )
      }
    }
  }
}

