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
import com.biosimilarity.evaluator.dsl.usage.ConcreteHL._
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

trait AgentCRUDHandler {
  self : EvaluationCommsService =>
 
  import DSLCommLink.mTT

  //## Methods on Sessions
  //### Ping and pong
  def handlesessionPing(
    msg : sessionPing
  ) : Unit = {
  }
  def handlesessionPong(
    msg : sessionPong
  ) : Unit = {
  }

  //## Methods on Agents
  //### createAgent
  def handlecreateAgentRequest(
    msg : createAgentRequest
  ) : Unit = {
  }
  //    - `authType == "password"` (case-insensitive)
  def handlecreateAgentError(
    msg : createAgentError
  ) : Unit = {
  }
  //    - returned synchronously
  def handlecreateAgentResponse(
    msg : createAgentResponse
  ) : Unit = {
  }
  //    - returned synchronously
  
  //### initializeSession
  def handleinitializeSessionRequest(
    msg : initializeSessionRequest
  ) : Unit = {
  }
  def handleinitializeSessionError(
    msg : initializeSessionError
  ) : Unit = {
  }
  //    - returned synchronously
  def handleinitializeSessionResponse(
    msg : initializeSessionResponse
  ) : Unit = {
  }
  //    - returned synchronously
  
  //### External identities
  //#### addAgentExternalIdentity
  def handleaddAgentExternalIdentityRequest[ID](
    msg : addAgentExternalIdentityRequest[ID]
  ) : Unit = {
  }
  //    - `ID(idType: IDType, idValue: String)`
  //        - `IDType = Email`
  //    - We only support adding one identity per message because of need for confirmation
  def handleaddAgentExternalIdentityError(
    msg : addAgentExternalIdentityError
  ) : Unit = {
  }
  def handleaddAgentExternalIdentityWaiting(
    msg : addAgentExternalIdentityWaiting
  ) : Unit = {
  }
  def handleaddAgentExternalIdentityToken(
    msg : addAgentExternalIdentityToken
  ) : Unit = {
  }
  def handleaddAgentExternalIdentityResponse(
    msg : addAgentExternalIdentityResponse
  ) : Unit = {
  }
  
  //#### removeAgentExternalIdentities
  def handleremoveAgentExternalIdentitiesRequest[ID](
    msg : removeAgentExternalIdentitiesRequest[ID]
  ) : Unit = {
  }
  def handleremoveAgentExternalIdentitiesError(
    msg : removeAgentExternalIdentitiesError
  ) : Unit = {
  }
  def handleremoveAgentExternalIdentitiesResponse(
    msg : removeAgentExternalIdentitiesResponse
  ) : Unit = {
  }
  
  //#### getAgentExternalIdentities
  def handlegetAgentExternalIdentitiesRequest[IDType](
    msg : getAgentExternalIdentitiesRequest[IDType]
  ) : Unit = {
  }
  //    - One value of `IDType` is `ANY`
  def handlegetAgentExternalIdentitiesError(
    msg : getAgentExternalIdentitiesError
  ) : Unit = {
  }
  def handlegetAgentExternalIdentitiesResponse[ID](
    msg : getAgentExternalIdentitiesResponse[ID]
  ) : Unit = {
  }
  
  //### Aliases
  //#### addAgentAliases
  def handleaddAgentAliasesRequest[Alias](
    msg : addAgentAliasesRequest[Alias]
  ) : Unit = {
  }
  //    - `Alias = String`
  def handleaddAgentAliasesError(
    msg : addAgentAliasesError
  ) : Unit
  = {
  }
  def handleaddAgentAliasesResponse(
    msg : addAgentAliasesResponse
  ) : Unit = {
  }
  
  //#### removeAgentAliases
  def handleremoveAgentAliasesRequest[Alias](
    msg : removeAgentAliasesRequest[Alias]
  ) : Unit = {
  }
  def handleremoveAgentAliasesError(
    msg : removeAgentAliasesError
  ) : Unit = {
  }
  def handleremoveAgentAliasesResponse(
    msg : removeAgentAliasesResponse
  ) : Unit = {
  }
  
  //#### getAgentAliases
  def handlegetAgentAliasesRequest(
    msg : getAgentAliasesRequest
  ) : Unit = {
  }
  def handlegetAgentAliasesError(
    msg : getAgentAliasesError
  ) : Unit = {
  }
  def handlegetAgentAliasesResponse[Alias](
    msg : getAgentAliasesResponse[Alias]
  ) : Unit = {
  }
  
  //#### getDefaultAlias
  def handlegetDefaultAliasRequest(
    msg : getDefaultAliasRequest
  ) : Unit = {
  }
  def handlegetDefaultAliasError(
    msg : getDefaultAliasError
  ) : Unit = {
  }
  def handlegetDefaultAliasResponse[Alias](
    msg : getDefaultAliasResponse[Alias]
  ) : Unit = {
  }
  
  //#### setDefaultAlias
  def handlesetDefaultAliasRequest[Alias](
    msg : setDefaultAliasRequest[Alias]
  ) : Unit = {
  }
  def handlesetDefaultAliasError(
    msg : setDefaultAliasError
  ) : Unit = {
  }
  def handlesetDefaultAliasResponse(
    msg : setDefaultAliasResponse
  ) : Unit = {
  }
  
  //## Methods on Aliases
  //### External identities
  //#### addAliasExternalIdentities
  def handleaddAliasExternalIdentitiesRequest[Alias,ID](
    msg : addAliasExternalIdentitiesRequest[Alias,ID]
  ) : Unit = {
  }
  //    - Only ids already on the agent are allowed
  def handleaddAliasExternalIdentitiesError(
    msg : addAliasExternalIdentitiesError
  ) : Unit = {
  }
  def handleaddAliasExternalIdentitiesResponse(
    msg : addAliasExternalIdentitiesResponse
  ) : Unit = {
  }
  
  //#### removeAliasExternalIdentities
  def handleremoveAliasExternalIdentitiesRequest[Alias,ID](
    msg : removeAliasExternalIdentitiesRequest[Alias,ID]
  ) : Unit = {
  }
  def handleremoveAliasExternalIdentitiesError(
    msg : removeAliasExternalIdentitiesError
  ) : Unit = {
  }
  def handleremoveAliasExternalIdentitiesResponse(
    msg : removeAliasExternalIdentitiesResponse
  ) : Unit = {
  }
  
  //#### getAliasExternalIdentities
  def handlegetAliasExternalIdentitiesRequest[Alias,IDType](
    msg : getAliasExternalIdentitiesRequest[Alias,IDType]
  ) : Unit = {
  }
  //    - One value of `IDType` is `ANY`
  def handlegetAliasExternalIdentitiesError(
    msg : getAliasExternalIdentitiesError
  ) : Unit = {
  }
  def handlegetAliasExternalIdentitiesResponse[IDType](
    msg : getAliasExternalIdentitiesResponse[IDType]
  ) : Unit = {
  }
  
  //#### setAliasDefaultExternalIdentity
  def handlesetAliasDefaultExternalIdentityRequest[Alias,ID](
    msg : setAliasDefaultExternalIdentityRequest[Alias,ID]
  ) : Unit = {
  }
  def handlesetAliasDefaultExternalIdentityError(
    msg : setAliasDefaultExternalIdentityError
  ) : Unit = {
  }
  def handlesetAliasDefaultExternalIdentityResponse(
    msg : setAliasDefaultExternalIdentityResponse
  ) : Unit = {
  }
  
  //### Connections
  //#### addAliasConnections
  def handleaddAliasConnectionsRequest[Alias,Cnxn](
    msg : addAliasConnectionsRequest[Alias,Cnxn]
  ) : Unit = {
  }
  //    - `Cnxn = (URI, FlatTerm, URI)`
  def handleaddAliasConnectionsError(
    msg : addAliasConnectionsError
  ) : Unit = {
  }
  def handleaddAliasConnectionsResponse(
    msg : addAliasConnectionsResponse
  ) : Unit = {
  }
  
  //#### removeAliasConnections
  def handleremoveAliasConnectionsRequest[Alias,Cnxn](
    msg : removeAliasConnectionsRequest[Alias,Cnxn]
  ) : Unit = {
  }
  def handleremoveAliasConnectionsError(
    msg : removeAliasConnectionsError
  ) : Unit = {
  }
  def handleremoveAliasConnectionsResponse(
    msg : removeAliasConnectionsResponse
  ) : Unit = {
  }
  
  //#### getAliasConnections
  def handlegetAliasConnectionsRequest[Alias](
    msg : getAliasConnectionsRequest[Alias]
  ) : Unit = {
  }
  def handlegetAliasConnectionsError(
    msg : getAliasConnectionsError
  ) : Unit = {
  }
  def handlegetAliasConnectionsResponse[Cnxn](
    msg : getAliasConnectionsResponse[Cnxn]
  ) : Unit = {
  }
  
  //#### setAliasDefaultConnection
  def handlesetAliasDefaultConnectionRequest[Alias,Cnxn](
    msg : setAliasDefaultConnectionRequest[Alias,Cnxn]
  ) : Unit = {
  }
  def handlesetAliasDefaultConnectionError(
    msg : setAliasDefaultConnectionError
  ) : Unit = {
  }
  def handlesetAliasDefaultConnectionResponse(
    msg : setAliasDefaultConnectionResponse
  ) : Unit = {
  }
  
  //### Labels
  //#### addAliasLabels
  def handleaddAliasLabelsRequest[Alias,Label](
    msg : addAliasLabelsRequest[Alias,Label]
  ) : Unit = {
  }
  //    - `Label = String`
  def handleaddAliasLabelsError(
    msg : addAliasLabelsError
  ) : Unit = {
  }
  def handleaddAliasLabelsResponse(
    msg : addAliasLabelsResponse
  ) : Unit = {
  }
  
  //#### removeAliasLabels
  def handleremoveAliasLabelsRequest[Alias,Label](
    msg : removeAliasLabelsRequest[Alias,Label]
  ) : Unit = {
  }
  def handleremoveAliasLabelsError(
    msg : removeAliasLabelsError
  ) : Unit = {
  }
  def handleremoveAliasLabelsResponse(
    msg : removeAliasLabelsResponse
  ) : Unit = {
  }
  
  //#### getAliasLabels
  def handlegetAliasLabelsRequest[Alias](
    msg : getAliasLabelsRequest[Alias]
  ) : Unit = {
  }
  def handlegetAliasLabelsError(
    msg : getAliasLabelsError
  ) : Unit = {
  }
  def handlegetAliasLabelsResponse[Label](
    msg : getAliasLabelsResponse[Label]
  ) : Unit = {
  }
  
  //#### setAliasDefaultLabel
  def handlesetAliasDefaultLabelRequest[Alias,Label](
    msg : setAliasDefaultLabelRequest[Alias,Label]
  ) : Unit = {
  }
  def handlesetAliasDefaultLabelError(
    msg : setAliasDefaultLabelError
  ) : Unit = {
  }
  def handlesetAliasDefaultLabelResponse(
    msg : setAliasDefaultLabelResponse
  ) : Unit = {
  }
  
  //#### getAliasDefaultLabel
  def handlegetAliasDefaultLabelRequest[Alias](
    msg : getAliasDefaultLabelRequest[Alias]
  ) : Unit = {
  }
  def handlegetAliasDefaultLabelError(
    msg : getAliasDefaultLabelError
  ) : Unit = {
  }
  def handlegetAliasDefaultLabelResponse[Label](
    msg : getAliasDefaultLabelResponse[Label]
  ) : Unit = {
  }
  
  //### DSL
  //#### evalSubscribe
  def handleevalSubscribeRequest[GloSExpr](
    msg : evalSubscribeRequest[GloSExpr]
  ) : Unit = {
  }
  //    - `GlosExpr =`
  //        - `InsertContent(Labels: List[Label], cnxns: List[Cnxn], value: Value)`
  //            - `Value = String`
  //        - `FeedExpr(Labels: List[Label], cnxns: List[Cnxn])`
  //        - `ScoreExpr(Labels: List[Label], cnxns: List[Cnxn], staff: Staff`
  //            - `Staff =`
  //                - `List[Cnxn]`
  //                - `List[Label]`
  def handleevalSubscribeError(
    msg : evalSubscribeError
  ) : Unit = {
  }
  def handleevalSubscribeResponse[Value](
    msg : evalSubscribeResponse[Value]
  ) : Unit = {
  }
  //- Can we know when we are done to send back an `evalSubscribeComplete`?
  
  //#### evalSubscribeCancel
  def handleevalSubscribeCancelRequest(
    msg : evalSubscribeCancelRequest
  ) : Unit = {
  }
  def handleevalSubscribeCancelError(
    msg : evalSubscribeCancelError
  ) : Unit = {
  }
  def handleevalSubscribeCancelResponse(
    msg : evalSubscribeCancelResponse
  ) : Unit = {
  }
}
