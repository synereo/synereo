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
      "agentURI",
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
  def handlecreateAgentError(
    key : String,
    msg : createAgentError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlecreateAgentError with msg : " + msg
    )
  }
  //    - returned synchronously
  
  //    - returned synchronously
  
  //### initializeSession
  def handleinitializeSessionRequest(
    key : String,
    msg : initializeSessionRequest
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleinitializeSessionRequest with msg : " + msg
    )
  }
  def handleinitializeSessionError(
    key : String,
    msg : initializeSessionError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleinitializeSessionError with msg : " + msg
    )
  }
  //    - returned synchronously
  
  //    - returned synchronously
  
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
  def handleaddAgentExternalIdentityError(
    key : String,
    msg : addAgentExternalIdentityError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAgentExternalIdentityError with msg : " + msg
    )
  }
  def handleaddAgentExternalIdentityWaiting(
    key : String,
    msg : addAgentExternalIdentityWaiting
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAgentExternalIdentityWaiting with msg : " + msg
    )
  }
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
  def handleremoveAgentExternalIdentitiesError(
    key : String,
    msg : removeAgentExternalIdentitiesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAgentExternalIdentitiesError with msg : "
  + msg
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
  def handlegetAgentExternalIdentitiesError(
    key : String,
    msg : getAgentExternalIdentitiesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetAgentExternalIdentitiesError with msg : " + msg
    )
  }
  def handlegetAgentExternalIdentitiesResponse[ID](
    key : String,
    msg : getAgentExternalIdentitiesResponse[ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handlegetAgentExternalIdentitiesResponse with msg : " + msg
    )
  }    

  //### Aliases
  //#### addAgentAliases
  def handleaddAgentAliasesRequest(
    key : String,
    msg : addAgentAliasesRequest
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleaddAgentAliasesRequest with msg : " + msg
    )
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
                CompletionMapper.complete(
                  key, 
                  compact(
                    render(
                      ( "msgType" -> "addAgentAliasesResponse" ) ~ ( "content" -> ( "sessionURI" -> msg.sessionURI.toString ) )
                    )
                  )
                )
              }
            v match {              
              case PostedExpr( previousAliasList : List[String] ) => {              
                val newAliasList = previousAliasList ++ msg.aliases
                BasicLogService.tweet("handleaddAgentAliasesRequest | onGet | onPut | updating aliasList with " + newAliasList )
                agentMgr().put[List[String]]( erql, erql )(
                  aliasStorageLocation, List( aliasStorageCnxn ), newAliasList, onPut
                )
              }
              case Bottom => {
                agentMgr().put[List[String]]( erql, erql )(
                  aliasStorageLocation, List( aliasStorageCnxn ), msg.aliases, onPut
                )
              }
            }
          }        
        }
      }
    
    agentMgr().get( erql, erql )( aliasStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  //    - `Alias = String`
  def handleaddAgentAliasesError(
    key : String,
    msg : addAgentAliasesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAgentAliasesError with msg : " + msg
    )
  }
  
  
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
                agentMgr().put[List[String]]( erql, erql )(
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

    agentMgr().get( erql, erql )( aliasStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  def handleremoveAgentAliasesError(
    key : String,
    msg : removeAgentAliasesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAgentAliasesError with msg : " + msg
    )
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

    agentMgr().get( erql, erql )( aliasStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  def handlegetAgentAliasesError(
    key : String,
    msg : getAgentAliasesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetAgentAliasesError with msg : " + msg
    )
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
  def handlegetDefaultAliasError(
    key : String,
    msg : getDefaultAliasError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetDefaultAliasError with msg : " + msg
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
  def handlesetDefaultAliasError(
    key : String,
    msg : setDefaultAliasError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlesetDefaultAliasError with msg : " + msg
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
  def handleaddAliasExternalIdentitiesError(
    key : String,
    msg : addAliasExternalIdentitiesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAliasExternalIdentitiesError with msg : " + msg
    )
  }
  
  
  //#### removeAliasExternalIdentities
  def handleremoveAliasExternalIdentitiesRequest[ID](
    key : String,
    msg : removeAliasExternalIdentitiesRequest[ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleremoveAliasExternalIdentitiesRequest with msg : " + msg
    )
  }
  def handleremoveAliasExternalIdentitiesError(
    key : String,
    msg : removeAliasExternalIdentitiesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAliasExternalIdentitiesError with msg : "
  + msg
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
  def handlegetAliasExternalIdentitiesError(
    key : String,
    msg : getAliasExternalIdentitiesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetAliasExternalIdentitiesError with msg : " + msg
    )
  }
  def handlegetAliasExternalIdentitiesResponse[IDType](
    key : String,
    msg : getAliasExternalIdentitiesResponse[IDType]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handlegetAliasExternalIdentitiesResponse with msg : " + msg
    )
  }
  
  //#### setAliasDefaultExternalIdentity
  def handlesetAliasDefaultExternalIdentityRequest[ID](
    key : String,
    msg : setAliasDefaultExternalIdentityRequest[ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handlesetAliasDefaultExternalIdentityRequest with msg : " + msg
    )
  }
  def handlesetAliasDefaultExternalIdentityError(
    key : String,
    msg : setAliasDefaultExternalIdentityError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlesetAliasDefaultExternalIdentityError with msg : " + msg
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
  def handleaddAliasConnectionsError(
    key : String,
    msg : addAliasConnectionsError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAliasConnectionsError with msg : " + msg
    )
  }
  
  
  //#### removeAliasConnections
  def handleremoveAliasConnectionsRequest[Cnxn](
    key : String,
    msg : removeAliasConnectionsRequest[Cnxn]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleremoveAliasConnectionsRequest with msg : " + msg
    )
  }
  def handleremoveAliasConnectionsError(
    key : String,
    msg : removeAliasConnectionsError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAliasConnectionsError with msg : " + msg
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
  def handlegetAliasConnectionsError(
    key : String,
    msg : getAliasConnectionsError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetAliasConnectionsError with msg : " + msg
    )
  }
  def handlegetAliasConnectionsResponse[Cnxn](
    key : String,
    msg : getAliasConnectionsResponse[Cnxn]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handlegetAliasConnectionsResponse with msg : " + msg
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
  def handlesetAliasDefaultConnectionError(
    key : String,
    msg : setAliasDefaultConnectionError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlesetAliasDefaultConnectionError with msg : " + msg
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
                CompletionMapper.complete(
                  key, 
                  compact(
                    render(
                      ( "msgType" -> "addAliasLabelsResponse" ) ~ ( "content" -> ( "sessionURI" -> msg.sessionURI.toString ) )
                    )
                  )
                )
              }
            v match {              
              case PostedExpr( previousLabelList : List[CnxnCtxtLabel[String,String,String]] ) => {              
                val newLabelList = previousLabelList ++ msg.labels
                BasicLogService.tweet("handleaddAliasLabelsRequest | onGet | onPut | updating labelList with " + newLabelList )
                agentMgr().put[List[CnxnCtxtLabel[String,String,String]]]( erql, erql )(
                  labelsStorageLocation, List( aliasStorageCnxn ), newLabelList, onPut
                )
              }
              case Bottom => {
                agentMgr().put[List[CnxnCtxtLabel[String,String,String]]]( erql, erql )(
                  labelsStorageLocation, List( aliasStorageCnxn ), msg.labels, onPut
                )
              }
            }
          }        
        }
      }
    
    agentMgr().get( erql, erql )( labelsStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  //    - `Label = String`
  def handleaddAliasLabelsError(
    key : String,
    msg : addAliasLabelsError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAliasLabelsError with msg : " + msg
    )
  }
  
  
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
                agentMgr().put[List[CnxnCtxtLabel[String,String,String]]]( erql, erql )(
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

    agentMgr().get( erql, erql )( labelsStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  def handleremoveAliasLabelsError(
    key : String,
    msg : removeAliasLabelsError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAliasLabelsError with msg : " + msg
    )
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

    agentMgr().get( erql, erql )( labelsStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  def handlegetAliasLabelsError(
    key : String,
    msg : getAliasLabelsError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetAliasLabelsError with msg : " + msg
    )
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
  def handlesetAliasDefaultLabelError(
    key : String,
    msg : setAliasDefaultLabelError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlesetAliasDefaultLabelError with msg : " + msg
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
  def handlegetAliasDefaultLabelError(
    key : String,
    msg : getAliasDefaultLabelError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetAliasDefaultLabelError with msg : " + msg
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
  def handleevalSubscribeError(
    key : String,
    msg : evalSubscribeError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleevalSubscribeError with msg : " + msg
    )
  }
  def handleevalSubscribeResponse[Value](
    key : String,
    msg : evalSubscribeResponse[Value]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeResponse with msg : " + msg
    )
  }
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
  def handleevalSubscribeCancelError(
    key : String,
    msg : evalSubscribeCancelError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleevalSubscribeCancelError with msg : " + msg
    )
  }
  
}
