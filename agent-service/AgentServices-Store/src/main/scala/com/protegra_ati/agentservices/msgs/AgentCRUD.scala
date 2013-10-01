// -*- mode: Scala;-*- 
// Filename:    AgentCRUD.scala 
// Authors:     lgm                                                    
// Creation:    Tue Oct  1 14:15:51 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.msgs.agent.crud

import java.util.UUID
import java.net.URI

//# Message Set

trait AgentCRUD

//## Methods on Sessions
//### Ping and pong
case class sessionPing(
  sessionURI : URI
) extends AgentCRUD
case class sessionPong(
  sessionURI : URI
) extends AgentCRUD

//## Methods on Agents
//### createAgent
case class createAgentRequest(
  authType : String,
  authValue : String
) extends AgentCRUD
//    - `authType == "password"` (case-insensitive)
case class createAgentError(
  reason : String
) extends AgentCRUD
//    - returned synchronously
case class createAgentResponse(
  agentURI : URI
) extends AgentCRUD
//    - returned synchronously

//### initializeSession
case class initializeSessionRequest(
  agentURI : URI
) extends AgentCRUD
case class initializeSessionError(
  agentURI : URI,
  reason : String
) extends AgentCRUD
//    - returned synchronously
case class initializeSessionResponse(
  sessionURI : URI
) extends AgentCRUD
//    - returned synchronously

//### External identities
//#### addAgentExternalIdentity
case class addAgentExternalIdentityRequest[ID](
  sessionURI : URI,
  id : ID
) extends AgentCRUD
//    - `ID(idType: IDType, idValue: String)`
//        - `IDType = Email`
//    - We only support adding one identity per message because of need for confirmation
case class addAgentExternalIdentityError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class addAgentExternalIdentityWaiting(
  sessionURI : URI
) extends AgentCRUD
case class addAgentExternalIdentityToken(
  sessionURI : URI,
  token : String
) extends AgentCRUD
case class addAgentExternalIdentityResponse(
  sessionURI : URI
) extends AgentCRUD

//#### removeAgentExternalIdentities
case class removeAgentExternalIdentitiesRequest[ID](
  sessionURI : URI,
  ids : List[ID]
) extends AgentCRUD
case class removeAgentExternalIdentitiesError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class removeAgentExternalIdentitiesResponse(
  sessionURI : URI
) extends AgentCRUD

//#### getAgentExternalIdentities
case class getAgentExternalIdentitiesRequest[IDType](
  sessionURI : URI,
  idType : IDType
) extends AgentCRUD
//    - One value of `IDType` is `ANY`
case class getAgentExternalIdentitiesError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class getAgentExternalIdentitiesResponse[ID](
  sessionURI : URI,
  ids : List[ID]
) extends AgentCRUD

//### Aliases
//#### addAgentAliases
case class addAgentAliasesRequest[Alias](
  sessionURI : URI,
  aliases : List[Alias]
) extends AgentCRUD
//    - `Alias = String`
case class addAgentAliasesError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class addAgentAliasesResponse(
  sessionURI : URI
) extends AgentCRUD

//#### removeAgentAliases
case class removeAgentAliasesRequest[Alias](
  sessionURI : URI,
  aliases : List[Alias]
) extends AgentCRUD
case class removeAgentAliasesError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class removeAgentAliasesResponse(
  sessionURI : URI
) extends AgentCRUD

//#### getAgentAliases
case class getAgentAliasesRequest( sessionURI : URI ) extends AgentCRUD
case class getAgentAliasesError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class getAgentAliasesResponse[Alias](
  sessionURI : URI,
  aliases : List[Alias]
) extends AgentCRUD

//#### getDefaultAlias
case class getDefaultAliasRequest(
  sessionURI : URI
) extends AgentCRUD
case class getDefaultAliasError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class getDefaultAliasResponse[Alias](
  sessionURI : URI,
  alias : Alias
) extends AgentCRUD

//#### setDefaultAlias
case class setDefaultAliasRequest[Alias](
  sessionURI : URI,
  alias : Alias
) extends AgentCRUD
case class setDefaultAliasError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class setDefaultAliasResponse(
  sessionURI : URI
) extends AgentCRUD

//## Methods on Aliases
//### External identities
//#### addAliasExternalIdentities
case class addAliasExternalIdentitiesRequest[Alias,ID](
  sessionURI : URI,
  alias : Alias,
  ids: List[ID]
) extends AgentCRUD
//    - Only ids already on the agent are allowed
case class addAliasExternalIdentitiesError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class addAliasExternalIdentitiesResponse(
  sessionURI : URI
) extends AgentCRUD

//#### removeAliasExternalIdentities
case class removeAliasExternalIdentitiesRequest[Alias,ID](
  sessionURI : URI,
  alias : Alias,
  ids : List[ID]
) extends AgentCRUD
case class removeAliasExternalIdentitiesError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class removeAliasExternalIdentitiesResponse(
  sessionURI : URI
) extends AgentCRUD

//#### getAliasExternalIdentities
case class getAliasExternalIdentitiesRequest[Alias,IDType](
  sessionURI : URI,
  alias : Alias,
  idType : IDType
) extends AgentCRUD
//    - One value of `IDType` is `ANY`
case class getAliasExternalIdentitiesError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class getAliasExternalIdentitiesResponse[IDType](
  sessionURI : URI,
  ids : List[(IDType, String)]
) extends AgentCRUD

//#### setAliasDefaultExternalIdentity
case class setAliasDefaultExternalIdentityRequest[Alias,ID](
  sessionURI : URI,
  alias : Alias,
  id : ID
) extends AgentCRUD
case class setAliasDefaultExternalIdentityError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class setAliasDefaultExternalIdentityResponse(
  sessionURI : URI
) extends AgentCRUD

//### Connections
//#### addAliasConnections
case class addAliasConnectionsRequest[Alias,Cnxn](
  sessionURI : URI,
  alias : Alias,
  cnxns : List[Cnxn]
) extends AgentCRUD
//    - `Cnxn = (URI, FlatTerm, URI)`
case class addAliasConnectionsError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class addAliasConnectionsResponse(
  sessionURI : URI
) extends AgentCRUD

//#### removeAliasConnections
case class removeAliasConnectionsRequest[Alias,Cnxn](
  sessionURI : URI,
  alias : Alias,
  cnxns : List[Cnxn]
) extends AgentCRUD
case class removeAliasConnectionsError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class removeAliasConnectionsResponse(
  sessionURI : URI
) extends AgentCRUD

//#### getAliasConnections
case class getAliasConnectionsRequest[Alias](
  sessionURI : URI,
  alias : Alias
) extends AgentCRUD
case class getAliasConnectionsError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class getAliasConnectionsResponse[Cnxn](
  sessionURI : URI,
  cnxns : List[Cnxn]
) extends AgentCRUD

//#### setAliasDefaultConnection
case class setAliasDefaultConnectionRequest[Alias,Cnxn](
  sessionURI : URI,
  alias : Alias,
  cnxn : Cnxn
) extends AgentCRUD
case class setAliasDefaultConnectionError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class setAliasDefaultConnectionResponse(
  sessionURI : URI
) extends AgentCRUD

//### Labels
//#### addAliasLabels
case class addAliasLabelsRequest[Alias,Label](
  sessionURI : URI,
  alias : Alias,
  Labels : List[Label]
) extends AgentCRUD
//    - `Label = String`
case class addAliasLabelsError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class addAliasLabelsResponse(
  sessionURI : URI
) extends AgentCRUD

//#### removeAliasLabels
case class removeAliasLabelsRequest[Alias,Label](
  sessionURI : URI,
  alias : Alias,
  Labels : List[Label]
) extends AgentCRUD
case class removeAliasLabelsError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class removeAliasLabelsResponse(
  sessionURI : URI
) extends AgentCRUD

//#### getAliasLabels
case class getAliasLabelsRequest[Alias](
  sessionURI : URI,
  alias : Alias
) extends AgentCRUD
case class getAliasLabelsError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class getAliasLabelsResponse[Label](
  sessionURI : URI,
  Labels : List[Label]
) extends AgentCRUD

//#### setAliasDefaultLabel
case class setAliasDefaultLabelRequest[Alias,Label](
  sessionURI : URI,
  alias : Alias,
  Label : Label
) extends AgentCRUD
case class setAliasDefaultLabelError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class setAliasDefaultLabelResponse(
  sessionURI : URI
) extends AgentCRUD

//#### getAliasDefaultLabel
case class getAliasDefaultLabelRequest[Alias](
  sessionURI : URI,
  alias : Alias
) extends AgentCRUD
case class getAliasDefaultLabelError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class getAliasDefaultLabelResponse[Label](
  sessionURI : URI,
  Label : Label
) extends AgentCRUD

//### DSL
//#### evalSubscribe
case class evalSubscribeRequest[GloSExpr](
  sessionURI : URI,
  expression : GloSExpr
) extends AgentCRUD
//    - `GlosExpr =`
//        - `InsertContent(Labels: List[Label], cnxns: List[Cnxn], value: Value)`
//            - `Value = String`
//        - `FeedExpr(Labels: List[Label], cnxns: List[Cnxn])`
//        - `ScoreExpr(Labels: List[Label], cnxns: List[Cnxn], staff: Staff`
//            - `Staff =`
//                - `List[Cnxn]`
//                - `List[Label]`
case class evalSubscribeError(
  sessionURI : URI, reason : String
) extends AgentCRUD
case class evalSubscribeResponse[Value](
  sessionURI : URI,
  values : List[Value]
) extends AgentCRUD
//- Can we know when we are done to send back an `evalSubscribeComplete`?

//#### evalSubscribeCancel
case class evalSubscribeCancelRequest(
  sessionURI : URI
) extends AgentCRUD
case class evalSubscribeCancelError(
  sessionURI : URI,
  reason : String
) extends AgentCRUD
case class evalSubscribeCancelResponse(
  sessionURI : URI
) extends AgentCRUD


