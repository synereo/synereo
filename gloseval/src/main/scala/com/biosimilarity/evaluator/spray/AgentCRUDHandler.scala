// -*- mode: Scala;-*- 
// Filename:    AgentCRUDHandler.scala 
// Authors:     lgm                                                    
// Creation:    Tue Oct  1 15:44:37 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.spray

import com.protegra_ati.agentservices.store._

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.msgs._
import com.biosimilarity.evaluator.msgs.agent.crud._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._

import akka.actor._
import spray.routing._
import directives.CompletionMagnet
import spray.http._
import spray.http.StatusCodes._
import MediaTypes._

import spray.httpx.encoding._

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import com.typesafe.config._

import javax.crypto._
import javax.crypto.spec.SecretKeySpec
import java.security._


import java.util.Date
import java.util.UUID

import java.net.URI

trait AgentCRUDSchema {
  self : EvaluationCommsService =>
 
  import DSLCommLink.mTT
  import ConcreteHL._

  var _aliasStorageLocation : Option[CnxnCtxtLabel[String,String,String]] = None
  def aliasStorageLocation() : CnxnCtxtLabel[String,String,String] = {
    _aliasStorageLocation match {
      case Some( asl ) => asl
      case None => {
        fromTermString(
          "aliasList( true )"
        ).getOrElse(
          throw new Exception( "Couldn't parse label: " + "aliasList( true )" )
        )          
      }
    }
  }
  
  var _labelsStorageLocation : Option[CnxnCtxtLabel[String,String,String]] = None
  def labelsStorageLocation() : CnxnCtxtLabel[String,String,String] = {
    _labelsStorageLocation match {
      case Some( lsl ) => lsl
      case None => {
        fromTermString(
          "labelsList( true )"
        ).getOrElse(
          throw new Exception( "Couldn't parse label: " + "labelsList( true )" )
        )          
      }
    }
  }

  var _cnxnsStorageLocation : Option[CnxnCtxtLabel[String,String,String]] = None
  def cnxnsStorageLocation() : CnxnCtxtLabel[String,String,String] = {
    _cnxnsStorageLocation match {
      case Some( csl ) => csl
      case None => {
        fromTermString(
          "cnxnsList( true )"
        ).getOrElse(
          throw new Exception( "Couldn't parse label: " + "cnxnsList( true )" )
        )          
      }
    }
  }

  def agentFromSession(
    sessionURI: URI
  ) : URI = {
    new URI(
      "agent",
      sessionURI.getUserInfo(),
      sessionURI.getAuthority(),
      sessionURI.getPort(),
      sessionURI.getPath(),
      sessionURI.getQuery(),
      sessionURI.getFragment()
    )    
  }
  def identityAliasFromAgent(
    agentURI : URI
  ) : PortableAgentCnxn = {
    PortableAgentCnxn(agentURI, "identity", agentURI)
  }  

  def getAliasCnxn(
    sessionURI : URI,
    aliasStr : String
  ) : PortableAgentCnxn = {
    val agentURI : URI = agentFromSession( sessionURI )
    PortableAgentCnxn( agentURI, aliasStr, agentURI )
  }    
  
}

trait AgentCRUDHandler extends AgentCRUDSchema {
  self : EvaluationCommsService =>
 
  import DSLCommLink.mTT
  import ConcreteHL._

  //## Methods on Sessions
  //### Ping and pong
  def handlesessionPing(
    msg : sessionPing
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlesessionPing with msg : " + msg
    )
  }
  def handlesessionPong(
    msg : sessionPong
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlesessionPong with msg : " + msg
    )
  }

  //## Methods on Agents
  //### createAgent
  def handlecreateAgentRequest(
    key : String,
    msg : createAgentRequest
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlecreateAgentRequest with msg : " + msg
    )
  }
  //    - `authType == "password"` (case-insensitive)
  
  //### initializeSession
  def handleinitializeSessionRequest(
    key : String,
    msg : initializeSessionRequest
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleinitializeSessionRequest with msg : " + msg
    )
  }
  
  //### External identities
  //#### addAgentExternalIdentity
  def handleaddAgentExternalIdentityRequest[ID](
    key : String,
    msg : addAgentExternalIdentityRequest[ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleaddAgentExternalIdentityRequest with msg : " + msg
    )
  }
  //    - `ID(idType: IDType, idValue: String)`
  //        - `IDType = Email`
  //    - We only support adding one identity per message because of need for confirmation
  def handleaddAgentExternalIdentityToken(
    key : String,
    msg : addAgentExternalIdentityToken
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAgentExternalIdentityToken with msg : " + msg
    )
  }
  
  
  //#### removeAgentExternalIdentities
  def handleremoveAgentExternalIdentitiesRequest[ID](
    key : String,
    msg : removeAgentExternalIdentitiesRequest[ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleremoveAgentExternalIdentitiesRequest with msg : " + msg
    )
  }
  
  //#### getAgentExternalIdentities
  def handlegetAgentExternalIdentitiesRequest[IDType](
    key : String,
    msg : getAgentExternalIdentitiesRequest[IDType]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handlegetAgentExternalIdentitiesRequest with msg : " + msg
    )
  }
  //    - One value of `IDType` is `ANY`

  //### Aliases
  //#### addAgentAliases
  def handleaddAgentAliasesRequest(
    key : String,
    msg : addAgentAliasesRequest
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleaddAgentAliasesRequest with msg : " + msg
    )
    val sessionURIStr = msg.sessionURI.toString
    val (erql, erspl) = agentMgr().makePolarizedPair()
    val aliasStorageCnxn =
      identityAliasFromAgent( agentFromSession( msg.sessionURI ) )
    val onGet : Option[mTT.Resource] => Unit = 
      ( optRsrc : Option[mTT.Resource] ) => {
        optRsrc match {
          case None => {
            // Nothing to be done
            BasicLogService.tweet("handleaddAgentAliasesRequest | onGet: got None")
          }
          case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
            BasicLogService.tweet("handleaddAgentAliasesRequest | onGet: got " + v )
            val onPut : Option[mTT.Resource] => Unit =
              ( optRsrc : Option[mTT.Resource] ) => {
                BasicLogService.tweet("handleaddAgentAliasesRequest | onGet | onPut")
                CometActorMapper.cometMessage(key, sessionURIStr, compact(render(
                  ( "msgType" -> "addAgentAliasesResponse" ) ~
                  ( "content" -> ( "sessionURI" -> sessionURIStr ) )
                )))
              }
            v match {              
              case PostedExpr( previousAliasList : List[String] ) => {              
                val newAliasList = previousAliasList ++ msg.aliases
                BasicLogService.tweet("handleaddAgentAliasesRequest | onGet | onPut | updating aliasList with " + newAliasList )
                agentMgr().put[List[String]]( erql, erspl )(
                  aliasStorageLocation, List( aliasStorageCnxn ), newAliasList, onPut
                )
              }
              case Bottom => {
                agentMgr().put[List[String]]( erql, erspl )(
                  aliasStorageLocation, List( aliasStorageCnxn ), msg.aliases, onPut
                )
              }
            }
          }
          case wonky => {
            CometActorMapper.cometMessage(key, sessionURIStr, compact(render(
              ("msgType" -> "addAgentAliasesError") ~
              ("content" -> ("reason" -> ("Got wonky response: " + wonky.toString)))
            )))
          }
        }
      }
    
    agentMgr().get( erql, erspl )( aliasStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  //    - `Alias = String`
  
  
  //#### removeAgentAliases
  def handleremoveAgentAliasesRequest(
    key : String,
    msg : removeAgentAliasesRequest
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleremoveAgentAliasesRequest with msg : " + msg
    )
    val (erql, erspl) = agentMgr().makePolarizedPair()
    val aliasStorageCnxn =
      identityAliasFromAgent( agentFromSession( msg.sessionURI ) )
    val onGet : Option[mTT.Resource] => Unit =
      ( optRsrc : Option[mTT.Resource] ) => {
        optRsrc match {
          case None => {
            // Nothing to be done
            BasicLogService.tweet("handleremoveAgentAliasesRequest | onGet: got None")
          }
          case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
            BasicLogService.tweet("handleremoveAgentAliasesRequest | onGet: got " + v )
            val onPut : Option[mTT.Resource] => Unit =
              ( optRsrc : Option[mTT.Resource] ) => {
                BasicLogService.tweet("handleremoveAgentAliasesRequest | onGet | onPut")
                CompletionMapper.complete(
                  key,
                  compact(
                    render(
                      ( "msgType" -> "removeAgentAliasesResponse" ) ~ ( "content" -> ( "sessionURI" -> msg.sessionURI.toString ) )
                    )
                  )
                )
              }
            v match {
              case PostedExpr( previousAliasList : List[String] ) => {
                val newAliasList = previousAliasList.filterNot(msg.aliases.contains)
                BasicLogService.tweet("handleremoveAgentAliasesRequest | onGet | onPut | updating aliasList with " + newAliasList )
                agentMgr().put[List[String]]( erql, erspl )(
                  aliasStorageLocation, List( aliasStorageCnxn ), newAliasList, onPut
                )
              }
              case Bottom => {
                BasicLogService.tweet("handleremoveAgentAliasesRequest | onGet: no aliasList exists")
              }
            }
          }
        }
      }

    agentMgr().get( erql, erspl )( aliasStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  
  
  //#### getAgentAliases
  def handlegetAgentAliasesRequest(
    key : String,
    msg : getAgentAliasesRequest
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetAgentAliasesRequest with msg : " + msg
    )
    val (erql, erspl) = agentMgr().makePolarizedPair()
    val aliasStorageCnxn =
      identityAliasFromAgent( agentFromSession( msg.sessionURI ) )
    val onGet : Option[mTT.Resource] => Unit =
      ( optRsrc : Option[mTT.Resource] ) => {
        optRsrc match {
          case None => {
            // Nothing to be done
            BasicLogService.tweet("handlegetAgentAliasesRequest | onGet: got None")
          }
          case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
            BasicLogService.tweet("handlegetAgentAliasesRequest | onGet: got " + v )
            val aliasList = v match {
              case PostedExpr( aliasList : List[String] ) => aliasList
              case Bottom => Nil
            }
            CompletionMapper.complete(
              key,
              compact(
                render(
                  ( "msgType" -> "getAgentAliasesResponse" ) ~
                  ( "content" ->
                    ( "sessionURI" -> msg.sessionURI.toString ) ~
                    ( "aliases" -> aliasList )
                  )
                )
              )
            )
          }
        }
      }

    agentMgr().get( erql, erspl )( aliasStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  
  
  //#### getDefaultAlias
  def handlegetDefaultAliasRequest(
    key : String,
    msg : getDefaultAliasRequest
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetDefaultAliasRequest with msg : " + msg
    )
  }
  
  
  //#### setDefaultAlias
  def handlesetDefaultAliasRequest(
    key : String,
    msg : setDefaultAliasRequest
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handlesetDefaultAliasRequest with msg : " + msg
    )
  }
  
  
  //## Methods on Aliases
  //### External identities
  //#### addAliasExternalIdentities
  def handleaddAliasExternalIdentitiesRequest[ID](
    key : String,
    msg : addAliasExternalIdentitiesRequest[ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleaddAliasExternalIdentitiesRequest with msg : " + msg
    )
  }
  //    - Only ids already on the agent are allowed
  
  
  //#### removeAliasExternalIdentities
  def handleremoveAliasExternalIdentitiesRequest[ID](
    key : String,
    msg : removeAliasExternalIdentitiesRequest[ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleremoveAliasExternalIdentitiesRequest with msg : " + msg
    )
  }
  
  
  //#### getAliasExternalIdentities
  def handlegetAliasExternalIdentitiesRequest[IDType](
    key : String,
    msg : getAliasExternalIdentitiesRequest[IDType]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handlegetAliasExternalIdentitiesRequest with msg : " + msg
    )
  }
  //    - One value of `IDType` is `ANY`
  
  //#### setAliasDefaultExternalIdentity
  def handlesetAliasDefaultExternalIdentityRequest[ID](
    key : String,
    msg : setAliasDefaultExternalIdentityRequest[ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handlesetAliasDefaultExternalIdentityRequest with msg : " + msg
    )
  }
  
  
  //### Connections
  //#### addAliasConnections
  def handleaddAliasConnectionsRequest[Cnxn](
    key : String,
    msg : addAliasConnectionsRequest[Cnxn]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleaddAliasConnectionsRequest with msg : " + msg
    )
  }
  //    - `Cnxn = (URI, FlatTerm, URI)`
  
  
  //#### removeAliasConnections
  def handleremoveAliasConnectionsRequest[Cnxn](
    key : String,
    msg : removeAliasConnectionsRequest[Cnxn]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleremoveAliasConnectionsRequest with msg : " + msg
    )
  }
  
  
  //#### getAliasConnections
  def handlegetAliasConnectionsRequest(
    key : String,
    msg : getAliasConnectionsRequest
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handlegetAliasConnectionsRequest with msg : " + msg
    )
  }
  
  //#### setAliasDefaultConnection
  def handlesetAliasDefaultConnectionRequest[Cnxn](
    key : String,
    msg : setAliasDefaultConnectionRequest[Cnxn]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handlesetAliasDefaultConnectionRequest with msg : " + msg
    )
  }
  
  //### Labels
  //#### addAliasLabels
  def handleaddAliasLabelsRequest(
    key : String,
    msg : addAliasLabelsRequest
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleaddAliasLabelsRequest with msg : " + msg
    )
    val sessionURIStr = msg.sessionURI.toString
    val (erql, erspl) = agentMgr().makePolarizedPair()
    val aliasStorageCnxn =
      getAliasCnxn( msg.sessionURI, msg.alias )
    val onGet : Option[mTT.Resource] => Unit = 
      ( optRsrc : Option[mTT.Resource] ) => {
        optRsrc match {
          case None => {
            // Nothing to be done
            BasicLogService.tweet(
              "handleaddAliasLabelsRequest | onGet: got None"
            )
          }
          case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
            BasicLogService.tweet(
              "handleaddAliasLabelsRequest | onGet: got " + v
            )
            val onPut : Option[mTT.Resource] => Unit =
              ( optRsrc : Option[mTT.Resource] ) => {
                BasicLogService.tweet("handleaddAliasLabelsRequest | onGet | onPut")
                CometActorMapper.cometMessage(key, sessionURIStr, compact(render(
                  ( "msgType" -> "addAliasLabelsResponse" ) ~ 
                  ( "content" -> ( "sessionURI" -> sessionURIStr ) )
                )))
              }
            v match {              
              case PostedExpr( previousLabelList : List[CnxnCtxtLabel[String,String,String]] ) => {              
                val newLabelList = previousLabelList ++ msg.labels
                BasicLogService.tweet("handleaddAliasLabelsRequest | onGet | onPut | updating labelList with " + newLabelList )
                agentMgr().put[List[CnxnCtxtLabel[String,String,String]]]( erql, erspl )(
                  labelsStorageLocation, List( aliasStorageCnxn ), newLabelList, onPut
                )
              }
              case Bottom => {
                agentMgr().put[List[CnxnCtxtLabel[String,String,String]]]( erql, erspl )(
                  labelsStorageLocation, List( aliasStorageCnxn ), msg.labels, onPut
                )
              }
            }
          }        
          case wonky => {
            CometActorMapper.cometMessage(key, sessionURIStr, compact(render(
              ( "msgType" -> "addAliasLabelsError" ) ~ 
              ( "content" -> ( "reason" -> ("Got wonky response: " + wonky) ) )
            )))
          }
        }
      }
    
    agentMgr().get( erql, erspl )( labelsStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  //    - `Label = String`
  
  
  //#### removeAliasLabels
  def handleremoveAliasLabelsRequest(
    key : String,
    msg : removeAliasLabelsRequest
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleremoveAliasLabelsRequest with msg : " + msg
    )
    val (erql, erspl) = agentMgr().makePolarizedPair()
    val aliasStorageCnxn =
      getAliasCnxn( msg.sessionURI, msg.alias )
    val onGet : Option[mTT.Resource] => Unit =
      ( optRsrc : Option[mTT.Resource] ) => {
        optRsrc match {
          case None => {
            // Nothing to be done
            BasicLogService.tweet(
              "handleremoveAliasLabelsRequest | onGet: got None"
            )
          }
          case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
            BasicLogService.tweet(
              "handleremoveAliasLabelsRequest | onGet: got " + v
            )
            val onPut : Option[mTT.Resource] => Unit =
              ( optRsrc : Option[mTT.Resource] ) => {
                BasicLogService.tweet("handleremoveAliasLabelsRequest | onGet | onPut")
                CompletionMapper.complete(
                  key,
                  compact(
                    render(
                      ( "msgType" -> "removeAliasLabelsResponse" ) ~ ( "content" -> ( "sessionURI" -> msg.sessionURI.toString ) )
                    )
                  )
                )
              }
            v match {
              case PostedExpr( previousLabelList : List[CnxnCtxtLabel[String,String,String]] ) => {
                val newLabelList = previousLabelList.filterNot(msg.labels.contains)
                BasicLogService.tweet("handleremoveAliasLabelsRequest | onGet | onPut | updating labelList with " + newLabelList )
                agentMgr().put[List[CnxnCtxtLabel[String,String,String]]]( erql, erspl )(
                  labelsStorageLocation, List( aliasStorageCnxn ), newLabelList, onPut
                )
              }
              case Bottom => {
                BasicLogService.tweet("handleremoveAliasLabelsRequest | onGet: no labelList exists")
              }
            }
          }
        }
      }

    agentMgr().get( erql, erspl )( labelsStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  
  
  //#### getAliasLabels
  def handlegetAliasLabelsRequest(
    key : String,
    msg : getAliasLabelsRequest
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handlegetAliasLabelsRequest with msg : " + msg
    )
    val (erql, erspl) = agentMgr().makePolarizedPair()
    val aliasStorageCnxn =
      getAliasCnxn( msg.sessionURI, msg.alias )
    val onGet : Option[mTT.Resource] => Unit =
      ( optRsrc : Option[mTT.Resource] ) => {
        optRsrc match {
          case None => {
            // Nothing to be done
            BasicLogService.tweet("handlegetAliasLabelsRequest | onGet: got None")
          }
          case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
            BasicLogService.tweet("handlegetAliasLabelsRequest | onGet: got " + v)
            val labelList = v match {
              case PostedExpr( labelList : List[CnxnCtxtLabel[String,String,String]] ) => labelList
              case Bottom => Nil
            }
            CompletionMapper.complete(
              key,
              compact(
                render(
                  ( "msgType" -> "getAliasLabelsResponse" ) ~
                  ( "content" ->
                    ( "sessionURI" -> msg.sessionURI.toString ) ~
                    ( "labels" -> labelList.map( l => l.toString ) )
                  )
                )
              )
            )
          }
        }
      }

    agentMgr().get( erql, erspl )( labelsStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  
  //#### setAliasDefaultLabel
  def handlesetAliasDefaultLabelRequest(
    key : String,
    msg : setAliasDefaultLabelRequest
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handlesetAliasDefaultLabelRequest with msg : " + msg
    )
  }
  
  
  //#### getAliasDefaultLabel
  def handlegetAliasDefaultLabelRequest(
    key : String,
    msg : getAliasDefaultLabelRequest
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handlegetAliasDefaultLabelRequest with msg : " + msg
    )
  }
  
  //### DSL
  //#### evalSubscribe
  def handleevalSubscribeRequest[GloSExpr](
    key : String,
    msg : evalSubscribeRequest[GloSExpr]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeRequest with msg : " + msg
    )
  }
  //    - `GlosExpr =`
  //        - `InsertContent(Labels: List, cnxns: List[Cnxn], value: Value)`
  //            - `Value = String`
  //        - `FeedExpr(Labels: List, cnxns: List[Cnxn])`
  //        - `ScoreExpr(Labels: List, cnxns: List[Cnxn], staff: Staff`
  //            - `Staff =`
  //                - `List[Cnxn]`
  //                - `List`
  //- Can we know when we are done to send back an `evalSubscribeComplete`?
  
  //#### evalSubscribeCancel
  def handleevalSubscribeCancelRequest(
    key : String,
    msg : evalSubscribeCancelRequest
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleevalSubscribeCancelRequest with msg : " + msg
    )
  }
}
