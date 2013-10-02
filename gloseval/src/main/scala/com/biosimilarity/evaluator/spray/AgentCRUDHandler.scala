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
    msg : createAgentRequest
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlecreateAgentRequest with msg : " + msg
    )
  }
  //    - `authType == "password"` (case-insensitive)
  def handlecreateAgentError(
    msg : createAgentError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlecreateAgentError with msg : " + msg
    )
  }
  //    - returned synchronously
  def handlecreateAgentResponse(
    msg : createAgentResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlecreateAgentResponse with msg : " + msg
    )
  }
  //    - returned synchronously
  
  //### initializeSession
  def handleinitializeSessionRequest(
    msg : initializeSessionRequest
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleinitializeSessionRequest with msg : " + msg
    )
  }
  def handleinitializeSessionError(
    msg : initializeSessionError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleinitializeSessionError with msg : " + msg
    )
  }
  //    - returned synchronously
  def handleinitializeSessionResponse(
    msg : initializeSessionResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleinitializeSessionResponse with msg : " + msg
    )
  }
  //    - returned synchronously
  
  //### External identities
  //#### addAgentExternalIdentity
  def handleaddAgentExternalIdentityRequest[ID](
    msg : addAgentExternalIdentityRequest[ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  //    - `ID(idType: IDType, idValue: String)`
  //        - `IDType = Email`
  //    - We only support adding one identity per message because of need for confirmation
  def handleaddAgentExternalIdentityError(
    msg : addAgentExternalIdentityError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAgentExternalIdentityError with msg : " + msg
    )
  }
  def handleaddAgentExternalIdentityWaiting(
    msg : addAgentExternalIdentityWaiting
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAgentExternalIdentityWaiting with msg : " + msg
    )
  }
  def handleaddAgentExternalIdentityToken(
    msg : addAgentExternalIdentityToken
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAgentExternalIdentityToken with msg : " + msg
    )
  }
  def handleaddAgentExternalIdentityResponse(
    msg : addAgentExternalIdentityResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAgentExternalIdentityResponse with msg : " + msg
    )
  }
  
  //#### removeAgentExternalIdentities
  def handleremoveAgentExternalIdentitiesRequest[ID](
    msg : removeAgentExternalIdentitiesRequest[ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  def handleremoveAgentExternalIdentitiesError(
    msg : removeAgentExternalIdentitiesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAgentExternalIdentitiesError with msg : "
  + msg
    )
  }
  def handleremoveAgentExternalIdentitiesResponse(
    msg : removeAgentExternalIdentitiesResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAgentExternalIdentitiesResponse with msg : " + msg
    )
  }
  
  //#### getAgentExternalIdentities
  def handlegetAgentExternalIdentitiesRequest[IDType](
    msg : getAgentExternalIdentitiesRequest[IDType]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  //    - One value of `IDType` is `ANY`
  def handlegetAgentExternalIdentitiesError(
    msg : getAgentExternalIdentitiesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetAgentExternalIdentitiesError with msg : " + msg
    )
  }
  def handlegetAgentExternalIdentitiesResponse[ID](
    msg : getAgentExternalIdentitiesResponse[ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  
  //### Aliases
  //#### addAgentAliases
  def handleaddAgentAliasesRequest[Alias](
    msg : addAgentAliasesRequest[Alias]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  //    - `Alias = String`
  def handleaddAgentAliasesError(
    msg : addAgentAliasesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAgentAliasesError with msg : " + msg
    )
  }
  def handleaddAgentAliasesResponse(
    msg : addAgentAliasesResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAgentAliasesResponse with msg : " + msg
    )
  }
  
  //#### removeAgentAliases
  def handleremoveAgentAliasesRequest[Alias](
    msg : removeAgentAliasesRequest[Alias]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  def handleremoveAgentAliasesError(
    msg : removeAgentAliasesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAgentAliasesError with msg : " + msg
    )
  }
  def handleremoveAgentAliasesResponse(
    msg : removeAgentAliasesResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAgentAliasesResponse with msg : " + msg
    )
  }
  
  //#### getAgentAliases
  def handlegetAgentAliasesRequest(
    msg : getAgentAliasesRequest
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetAgentAliasesRequest with msg : " + msg
    )
  }
  def handlegetAgentAliasesError(
    msg : getAgentAliasesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetAgentAliasesError with msg : " + msg
    )
  }
  def handlegetAgentAliasesResponse[Alias](
    msg : getAgentAliasesResponse[Alias]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  
  //#### getDefaultAlias
  def handlegetDefaultAliasRequest(
    msg : getDefaultAliasRequest
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetDefaultAliasRequest with msg : " + msg
    )
  }
  def handlegetDefaultAliasError(
    msg : getDefaultAliasError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetDefaultAliasError with msg : " + msg
    )
  }
  def handlegetDefaultAliasResponse[Alias](
    msg : getDefaultAliasResponse[Alias]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  
  //#### setDefaultAlias
  def handlesetDefaultAliasRequest[Alias](
    msg : setDefaultAliasRequest[Alias]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  def handlesetDefaultAliasError(
    msg : setDefaultAliasError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlesetDefaultAliasError with msg : " + msg
    )
  }
  def handlesetDefaultAliasResponse(
    msg : setDefaultAliasResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlesetDefaultAliasResponse with msg : " + msg
    )
  }
  
  //## Methods on Aliases
  //### External identities
  //#### addAliasExternalIdentities
  def handleaddAliasExternalIdentitiesRequest[Alias,ID](
    msg : addAliasExternalIdentitiesRequest[Alias,ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  //    - Only ids already on the agent are allowed
  def handleaddAliasExternalIdentitiesError(
    msg : addAliasExternalIdentitiesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAliasExternalIdentitiesError with msg : " + msg
    )
  }
  def handleaddAliasExternalIdentitiesResponse(
    msg : addAliasExternalIdentitiesResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAliasExternalIdentitiesResponse with msg : "
  + msg
    )
  }
  
  //#### removeAliasExternalIdentities
  def handleremoveAliasExternalIdentitiesRequest[Alias,ID](
    msg : removeAliasExternalIdentitiesRequest[Alias,ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  def handleremoveAliasExternalIdentitiesError(
    msg : removeAliasExternalIdentitiesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAliasExternalIdentitiesError with msg : "
  + msg
    )
  }
  def handleremoveAliasExternalIdentitiesResponse(
    msg : removeAliasExternalIdentitiesResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAliasExternalIdentitiesResponse with msg : " + msg
    )
  }
  
  //#### getAliasExternalIdentities
  def handlegetAliasExternalIdentitiesRequest[Alias,IDType](
    msg : getAliasExternalIdentitiesRequest[Alias,IDType]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  //    - One value of `IDType` is `ANY`
  def handlegetAliasExternalIdentitiesError(
    msg : getAliasExternalIdentitiesError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetAliasExternalIdentitiesError with msg : " + msg
    )
  }
  def handlegetAliasExternalIdentitiesResponse[IDType](
    msg : getAliasExternalIdentitiesResponse[IDType]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  
  //#### setAliasDefaultExternalIdentity
  def handlesetAliasDefaultExternalIdentityRequest[Alias,ID](
    msg : setAliasDefaultExternalIdentityRequest[Alias,ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  def handlesetAliasDefaultExternalIdentityError(
    msg : setAliasDefaultExternalIdentityError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlesetAliasDefaultExternalIdentityError with msg : " + msg
    )
  }
  def handlesetAliasDefaultExternalIdentityResponse(
    msg : setAliasDefaultExternalIdentityResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlesetAliasDefaultExternalIdentityResponse with msg : " + msg
    )
  }
  
  //### Connections
  //#### addAliasConnections
  def handleaddAliasConnectionsRequest[Alias,Cnxn](
    msg : addAliasConnectionsRequest[Alias,Cnxn]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  //    - `Cnxn = (URI, FlatTerm, URI)`
  def handleaddAliasConnectionsError(
    msg : addAliasConnectionsError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAliasConnectionsError with msg : " + msg
    )
  }
  def handleaddAliasConnectionsResponse(
    msg : addAliasConnectionsResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAliasConnectionsResponse with msg : " + msg
    )
  }
  
  //#### removeAliasConnections
  def handleremoveAliasConnectionsRequest[Alias,Cnxn](
    msg : removeAliasConnectionsRequest[Alias,Cnxn]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  def handleremoveAliasConnectionsError(
    msg : removeAliasConnectionsError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAliasConnectionsError with msg : " + msg
    )
  }
  def handleremoveAliasConnectionsResponse(
    msg : removeAliasConnectionsResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAliasConnectionsResponse with msg : " + msg
    )
  }
  
  //#### getAliasConnections
  def handlegetAliasConnectionsRequest[Alias](
    msg : getAliasConnectionsRequest[Alias]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  def handlegetAliasConnectionsError(
    msg : getAliasConnectionsError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetAliasConnectionsError with msg : " + msg
    )
  }
  def handlegetAliasConnectionsResponse[Cnxn](
    msg : getAliasConnectionsResponse[Cnxn]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  
  //#### setAliasDefaultConnection
  def handlesetAliasDefaultConnectionRequest[Alias,Cnxn](
    msg : setAliasDefaultConnectionRequest[Alias,Cnxn]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  def handlesetAliasDefaultConnectionError(
    msg : setAliasDefaultConnectionError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlesetAliasDefaultConnectionError with msg : " + msg
    )
  }
  def handlesetAliasDefaultConnectionResponse(
    msg : setAliasDefaultConnectionResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlesetAliasDefaultConnectionResponse with msg : " + msg
    )
  }
  
  //### Labels
  //#### addAliasLabels
  def handleaddAliasLabelsRequest[Alias,Label](
    msg : addAliasLabelsRequest[Alias,Label]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  //    - `Label = String`
  def handleaddAliasLabelsError(
    msg : addAliasLabelsError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAliasLabelsError with msg : " + msg
    )
  }
  def handleaddAliasLabelsResponse(
    msg : addAliasLabelsResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAliasLabelsResponse with msg : " + msg
    )
  }
  
  //#### removeAliasLabels
  def handleremoveAliasLabelsRequest[Alias,Label](
    msg : removeAliasLabelsRequest[Alias,Label]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  def handleremoveAliasLabelsError(
    msg : removeAliasLabelsError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAliasLabelsError with msg : " + msg
    )
  }
  def handleremoveAliasLabelsResponse(
    msg : removeAliasLabelsResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleremoveAliasLabelsResponse with msg : " + msg
    )
  }
  
  //#### getAliasLabels
  def handlegetAliasLabelsRequest[Alias](
    msg : getAliasLabelsRequest[Alias]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  def handlegetAliasLabelsError(
    msg : getAliasLabelsError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetAliasLabelsError with msg : " + msg
    )
  }
  def handlegetAliasLabelsResponse[Label](
    msg : getAliasLabelsResponse[Label]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  
  //#### setAliasDefaultLabel
  def handlesetAliasDefaultLabelRequest[Alias,Label](
    msg : setAliasDefaultLabelRequest[Alias,Label]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  def handlesetAliasDefaultLabelError(
    msg : setAliasDefaultLabelError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlesetAliasDefaultLabelError with msg : " + msg
    )
  }
  def handlesetAliasDefaultLabelResponse(
    msg : setAliasDefaultLabelResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlesetAliasDefaultLabelResponse with msg : " + msg
    )
  }
  
  //#### getAliasDefaultLabel
  def handlegetAliasDefaultLabelRequest[Alias](
    msg : getAliasDefaultLabelRequest[Alias]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  def handlegetAliasDefaultLabelError(
    msg : getAliasDefaultLabelError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlegetAliasDefaultLabelError with msg : " + msg
    )
  }
  def handlegetAliasDefaultLabelResponse[Label](
    msg : getAliasDefaultLabelResponse[Label]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  
  //### DSL
  //#### evalSubscribe
  def handleevalSubscribeRequest[GloSExpr](
    msg : evalSubscribeRequest[GloSExpr]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
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
    BasicLogService.tweet( 
      "Entering: handleevalSubscribeError with msg : " + msg
    )
  }
  def handleevalSubscribeResponse[Value](
    msg : evalSubscribeResponse[Value]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
  //- Can we know when we are done to send back an `evalSubscribeComplete`?
  
  //#### evalSubscribeCancel
  def handleevalSubscribeCancelRequest(
    msg : evalSubscribeCancelRequest
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleevalSubscribeCancelRequest with msg : " + msg
    )
  }
  def handleevalSubscribeCancelError(
    msg : evalSubscribeCancelError
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleevalSubscribeCancelError with msg : " + msg
    )
  }
  def handleevalSubscribeCancelResponse(
    msg : evalSubscribeCancelResponse
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleevalSubscribeCancelResponse with msg : " + msg
    )
  }
}
