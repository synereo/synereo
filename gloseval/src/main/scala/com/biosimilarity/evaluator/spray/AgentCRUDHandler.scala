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

  var _defaultAliasStorageLocation : Option[CnxnCtxtLabel[String,String,String]] = None
  def defaultAliasStorageLocation() : CnxnCtxtLabel[String,String,String] = {
    _defaultAliasStorageLocation match {
      case Some( dasl ) => dasl
      case None => {
        fromTermString(
          "defaultAlias( true )"
        ).getOrElse(
          throw new Exception( "Couldn't parse label: " + "defaultAlias( true )" )
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
          "labelList( true )"
        ).getOrElse(
          throw new Exception( "Couldn't parse label: " + "labelList( true )" )
        )          
      }
    }
  }

  var _defaultLabelStorageLocation : Option[CnxnCtxtLabel[String,String,String]] = None
  def defaultLabelStorageLocation() : CnxnCtxtLabel[String,String,String] = {
    _defaultLabelStorageLocation match {
      case Some( dlsl ) => dlsl
      case None => {
        fromTermString(
          "defaultLabel( true )"
        ).getOrElse(
          throw new Exception( "Couldn't parse label: " + "defaultLabel( true )" )
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
    BasicLogService.tweet( "Entering: handleaddAgentAliasesRequest with msg : " + msg )

    val aliasStorageCnxn = identityAliasFromAgent( agentFromSession( msg.sessionURI ) )

    val onGet : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      val sessionURIStr = msg.sessionURI.toString

      optRsrc match {
        case None => {
          // Nothing to be done
          BasicLogService.tweet( "handleaddAgentAliasesRequest | onGet: got None" )
        }
        case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
          BasicLogService.tweet( "handleaddAgentAliasesRequest | onGet: got " + v )

          val onPut : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
            BasicLogService.tweet( "handleaddAgentAliasesRequest | onGet | onPut" )

            CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
              ( "msgType" -> "addAgentAliasesResponse" ) ~
              ( "content" -> ( "sessionURI" -> sessionURIStr ) )
            ) ) )
          }

          v match {              
            case PostedExpr( previousAliasList : String ) => {
              val newAliasList = compact( render( parse( previousAliasList ) ++ msg.aliases ) )

              BasicLogService.tweet( "handleaddAgentAliasesRequest | onGet | onPut | updating aliasList with " + newAliasList )

              agentMgr().put[String]( aliasStorageLocation, List( aliasStorageCnxn ), newAliasList, onPut )
            }
            case Bottom => {
              agentMgr().put[String]( aliasStorageLocation, List( aliasStorageCnxn ), compact(render(msg.aliases)), onPut )
            }
          }
        }
        case wonky => {
          CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
            ( "msgType" -> "addAgentAliasesError" ) ~
            ( "content" ->
              ( "sessionURI" -> sessionURIStr ) ~
              ( "reason" -> ( "Got wonky response: " + wonky.toString ) )
            )
          ) ) )
        }
      }
    }
      
    agentMgr().get( aliasStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  //    - `Alias = String`
  
  
  //#### removeAgentAliases
  def handleremoveAgentAliasesRequest(
    key : String,
    msg : removeAgentAliasesRequest
  ) : Unit = {
    BasicLogService.tweet( "Entering: handleremoveAgentAliasesRequest with msg : " + msg )

    val aliasStorageCnxn = identityAliasFromAgent( agentFromSession( msg.sessionURI ) )

    val onGet : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      optRsrc match {
        case None => {
          // Nothing to be done
          BasicLogService.tweet( "handleremoveAgentAliasesRequest | onGet: got None" )
        }
        case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
          BasicLogService.tweet( "handleremoveAgentAliasesRequest | onGet: got " + v )

          val sessionURIStr = msg.sessionURI.toString

          val onPut : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
            BasicLogService.tweet("handleremoveAgentAliasesRequest | onGet | onPut")

            CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
              ( "msgType" -> "removeAgentAliasesResponse" ) ~
              ( "content" -> ( "sessionURI" -> sessionURIStr ) )
            ) ) )
          }

          v match {
            case PostedExpr( previousAliasList : List[String] ) => {
              val newAliasList = previousAliasList.filterNot( msg.aliases.contains )

              BasicLogService.tweet(
                "handleremoveAgentAliasesRequest | onGet | onPut | updating aliasList with " + newAliasList
              )

              agentMgr().put[List[String]]( aliasStorageLocation, List( aliasStorageCnxn ), newAliasList, onPut )
            }
            case Bottom => {
              BasicLogService.tweet( "handleremoveAgentAliasesRequest | onGet: no aliasList exists" )

              CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
                ( "msgType" -> "removeAgentAliasesError" ) ~
                ( "content" ->
                  ( "sessionURI" -> sessionURIStr ) ~
                  ( "reason" -> "no aliasList exists" )
                )
              ) ) )
            }
          }
        }
      }
    }

    agentMgr().get( aliasStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  
  
  //#### getAgentAliases
  def handlegetAgentAliasesRequest(
    key : String,
    msg : getAgentAliasesRequest
  ) : Unit = {
    BasicLogService.tweet( "Entering: handlegetAgentAliasesRequest with msg : " + msg )

    val aliasStorageCnxn = identityAliasFromAgent( agentFromSession( msg.sessionURI ) )

    val onFetch : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      optRsrc match {
        case None => {
          // Nothing to be done
            BasicLogService.tweet( "handlegetAgentAliasesRequest | onFetch: got None" )
        }
        case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
          BasicLogService.tweet( "handlegetAgentAliasesRequest | onFetch: got " + v )

          val sessionURIStr = msg.sessionURI.toString

          val aliasList = v match {
            case PostedExpr( aliasList : List[String] ) => aliasList
            case Bottom => Nil
          }

          CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
            ( "msgType" -> "getAgentAliasesResponse" ) ~
            ( "content" ->
              ( "sessionURI" -> sessionURIStr ) ~
              ( "aliases" -> aliasList )
            )
          ) ) )
        }
      }
    }
    
    agentMgr().fetch( aliasStorageLocation, List( aliasStorageCnxn ), onFetch )
  }
  
  
  //#### getDefaultAlias
  def handlegetDefaultAliasRequest(
    key : String,
    msg : getDefaultAliasRequest
  ) : Unit = {
    BasicLogService.tweet( "Entering: handlegetDefaultAliasRequest with msg : " + msg )

    val aliasStorageCnxn = identityAliasFromAgent( agentFromSession( msg.sessionURI ) )

    val onFetch : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
        optRsrc match {
          case None => {
            // Nothing to be done
            BasicLogService.tweet( "handlegetDefaultAliasRequest | onFetch: got None" )
          }
          case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
            BasicLogService.tweet( "handlegetDefaultAliasRequest | onFetch: got " + v )

            val sessionURIStr = msg.sessionURI.toString

            v match {
              case PostedExpr( defaultAlias : String ) => {
                CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
                  ( "msgType" -> "getDefaultAliasResponse" ) ~
                  ( "content" ->
                    ( "sessionURI" -> sessionURIStr ) ~
                    ( "alias" -> defaultAlias )
                  )
                ) ) )
              }
              case Bottom => {
                CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
                  ( "msgType" -> "getDefaultAliasError" ) ~
                  ( "content" ->
                    ( "sessionURI" -> sessionURIStr ) ~
                    ( "reason" -> "No default alias exists" )
                  )
                ) ) )
              }
            }
          }
        }
      }

    agentMgr().fetch( defaultAliasStorageLocation, List( aliasStorageCnxn ), onFetch )
  }
  
  
  //#### setDefaultAlias
  def handlesetDefaultAliasRequest(
    key : String,
    msg : setDefaultAliasRequest
  ) : Unit = {
    BasicLogService.tweet( "Entering: handlesetDefaultAliasRequest with msg : " + msg )

    val aliasStorageCnxn = identityAliasFromAgent( agentFromSession( msg.sessionURI ) )

    val onPut : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      BasicLogService.tweet( "handlesetDefaultAliasRequest | onPut" )

      val sessionURIStr = msg.sessionURI.toString

      CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
        ( "msgType" -> "setDefaultAliasResponse" ) ~
        ( "content" -> ( "sessionURI" -> sessionURIStr ) )
      ) ) )
    }

    agentMgr().put[String]( defaultAliasStorageLocation, List( aliasStorageCnxn ), msg.alias, onPut )
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
    BasicLogService.tweet( "Entering: handleaddAliasConnectionsRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )

    val onGet : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      optRsrc match {
        case None => {
          // Nothing to be done
          BasicLogService.tweet( "handleaddAliasConnectionsRequest | onGet: got None" )
        }
        case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
          BasicLogService.tweet( "handleaddAliasConnectionsRequest | onGet: got " + v )

          val onPut : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
            BasicLogService.tweet( "handleaddAliasConnectionsRequest | onGet | onPut" )

            val sessionURIStr = msg.sessionURI.toString

            CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
              ( "msgType" -> "addAliasConnectionsResponse" ) ~
              ( "content" -> ( "sessionURI" -> sessionURIStr ) )
            ) ) )

          }
          v match {
            case PostedExpr( previousCnxnList : List[Cnxn] ) => {
              val newCnxnList = previousCnxnList ++ msg.cnxns

              BasicLogService.tweet(
                "handleaddAliasConnectionsRequest | onGet | onPut | updating cnxnList with " + newCnxnList
              )

              agentMgr().put[List[Cnxn]]( cnxnsStorageLocation, List( aliasStorageCnxn ), newCnxnList, onPut )
            }
            case Bottom => {
              agentMgr().put[List[Cnxn]]( cnxnsStorageLocation, List( aliasStorageCnxn ), msg.cnxns, onPut )
            }
          }
        }
      }
    }

    agentMgr().get( cnxnsStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  //    - `Cnxn = (URI, FlatTerm, URI)`
  
  
  //#### removeAliasConnections
  def handleremoveAliasConnectionsRequest[Cnxn](
    key : String,
    msg : removeAliasConnectionsRequest[Cnxn]
  ) : Unit = {
    BasicLogService.tweet( "Entering: handleremoveAliasConnectionsRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )

    val onGet : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      optRsrc match {
        case None => {
          // Nothing to be done
          BasicLogService.tweet( "handleremoveAliasConnectionsRequest | onGet: got None" )
        }
        case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
          BasicLogService.tweet( "handleremoveAliasConnectionsRequest | onGet: got " + v )

          val sessionURIStr = msg.sessionURI.toString

          val onPut : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
            BasicLogService.tweet( "handleremoveAliasConnectionsRequest | onGet | onPut" )

            CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
              ( "msgType" -> "removeAliasConnectionsResponse" ) ~
              ( "content" -> ( "sessionURI" -> sessionURIStr ) )
            ) ) )
          }

          v match {
            case PostedExpr( previousCnxnList : List[Cnxn] ) => {
              val newCnxnList = previousCnxnList.filterNot( msg.cnxns.contains )

              BasicLogService.tweet(
                "handleremoveAliasConnectionsRequest | onGet | onPut | updating cnxnList with " + newCnxnList
              )

              agentMgr().put[List[Cnxn]]( cnxnsStorageLocation, List( aliasStorageCnxn ), newCnxnList, onPut )
            }
            case Bottom => {
              BasicLogService.tweet( "handleremoveAliasConnectionsRequest | onGet: no cnxnList exists" )

              CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
                ( "msgType" -> "removeAliasConnectionsError" ) ~
                ( "content" ->
                  ( "sessionURI" -> sessionURIStr ) ~
                  ( "reason" -> "no cnxnList exists" )
                )
              ) ) )
            }
          }
        }
      }
    }

    agentMgr().get( cnxnsStorageLocation, List( aliasStorageCnxn ), onGet )
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
    BasicLogService.tweet( "Entering: handleaddAliasLabelsRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )

    val onGet : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      val sessionURIStr = msg.sessionURI.toString

      optRsrc match {
        case None => {
          // Nothing to be done
          BasicLogService.tweet( "handleaddAliasLabelsRequest | onGet: got None" )
        }
        case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
          BasicLogService.tweet( "handleaddAliasLabelsRequest | onGet: got " + v )

          val onPut : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
            BasicLogService.tweet( "handleaddAliasLabelsRequest | onGet | onPut" )

            CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
              ( "msgType" -> "addAliasLabelsResponse" ) ~
              ( "content" -> ( "sessionURI" -> sessionURIStr ) )
            ) ) )
          }

          v match {
            case PostedExpr( previousLabelList : String ) => {
              val newLabelList = compact( render( parse( previousLabelList ) ++ msg.labels.map( _.toString ) ) )

              BasicLogService.tweet(
                "handleaddAliasLabelsRequest | onGet | onPut | updating labelList with " + newLabelList
              )

              agentMgr().put[String]( labelsStorageLocation, List( aliasStorageCnxn ), newLabelList, onPut )
            }
            case Bottom => {
              val newLabelList = compact( render( msg.labels.map( _.toString ) ) )
              agentMgr().put[String]( labelsStorageLocation, List( aliasStorageCnxn ), newLabelList, onPut )
            }
          }
        }        
        case wonky => {
          CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
            ( "msgType" -> "addAliasLabelsError" ) ~ 
            ( "content" -> ( "reason" -> ("Got wonky response: " + wonky) ) )
          ) ) )
        }
      }
    }
    
    agentMgr().get( labelsStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  //    - `Label = String`
  
  
  //#### removeAliasLabels
  def handleremoveAliasLabelsRequest(
    key : String,
    msg : removeAliasLabelsRequest
  ) : Unit = {
    BasicLogService.tweet( "Entering: handleremoveAliasLabelsRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )

    val onGet : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      optRsrc match {
        case None => {
          // Nothing to be done
          BasicLogService.tweet( "handleremoveAliasLabelsRequest | onGet: got None" )
        }
        case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
          BasicLogService.tweet( "handleremoveAliasLabelsRequest | onGet: got " + v )

          val sessionURIStr = msg.sessionURI.toString

          val onPut : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
            BasicLogService.tweet( "handleremoveAliasLabelsRequest | onGet | onPut" )

            CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
              ( "msgType" -> "removeAliasLabelsResponse" ) ~
              ( "content" -> ( "sessionURI" -> sessionURIStr ) )
            ) ) )
          }

          v match {
            case PostedExpr( previousLabelList : List[CnxnCtxtLabel[String,String,String]] ) => {
              val newLabelList = previousLabelList.filterNot( msg.labels.contains )

              BasicLogService.tweet(
                "handleremoveAliasLabelsRequest | onGet | onPut | updating labelList with " + newLabelList
              )

              agentMgr().put[List[CnxnCtxtLabel[String,String,String]]](
                labelsStorageLocation,
                List( aliasStorageCnxn ),
                newLabelList,
                onPut
              )
            }
            case Bottom => {
              BasicLogService.tweet( "handleremoveAliasLabelsRequest | onGet: no labelList exists" )

              CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
                ( "msgType" -> "removeAliasLabelsError" ) ~
                ( "content" ->
                  ( "sessionURI" -> sessionURIStr ) ~
                  ( "reason" -> "no labelList exists" )
                )
              ) ) )
            }
          }
        }
      }
    }

    agentMgr().get( labelsStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  
  
  //#### getAliasLabels
  def handlegetAliasLabelsRequest(
    key : String,
    msg : getAliasLabelsRequest
  ) : Unit = {
    BasicLogService.tweet( "Entering: handlegetAliasLabelsRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )

    val onFetch : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      optRsrc match {
        case None => {
          // Nothing to be done
          BasicLogService.tweet( "handlegetAliasLabelsRequest | onFetch: got None" )
        }
        case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
          BasicLogService.tweet( "handlegetAliasLabelsRequest | onFetch: got " + v )

          val sessionURIStr = msg.sessionURI.toString

          val labelList = v match {
            case PostedExpr( labelList : List[CnxnCtxtLabel[String,String,String]] ) => labelList
            case Bottom => Nil
          }

          CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
            ( "msgType" -> "getAliasLabelsResponse" ) ~
            ( "content" ->
              ( "sessionURI" -> sessionURIStr ) ~
              ( "labels" -> labelList.map( l => l.toString ) )
            )
          ) ) )
        }
      }
    }

    agentMgr().fetch( labelsStorageLocation, List( aliasStorageCnxn ), onFetch )
  }
  
  //#### setAliasDefaultLabel
  def handlesetAliasDefaultLabelRequest(
    key : String,
    msg : setAliasDefaultLabelRequest
  ) : Unit = {
    BasicLogService.tweet( "Entering: handlesetAliasDefaultLabelRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )

    val onPut : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      BasicLogService.tweet( "handlesetAliasDefaultLabelRequest | onPut" )

      val sessionURIStr = msg.sessionURI.toString

      CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
        ( "msgType" -> "setAliasDefaultLabelResponse" ) ~
        ( "content" -> ( "sessionURI" -> sessionURIStr ) )
      ) ) )
    }

    agentMgr().put[CnxnCtxtLabel[String,String,String]](
      defaultLabelStorageLocation,
      List( aliasStorageCnxn ),
      msg.label,
      onPut
    )
  }
  
  
  //#### getAliasDefaultLabel
  def handlegetAliasDefaultLabelRequest(
    key : String,
    msg : getAliasDefaultLabelRequest
  ) : Unit = {
    BasicLogService.tweet( "Entering: handlegetAliasDefaultLabelRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )

    val onFetch : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      optRsrc match {
        case None => {
          // Nothing to be done
          BasicLogService.tweet( "handlegetAliasDefaultLabelRequest | onFetch: got None" )
        }
        case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
          BasicLogService.tweet( "handlegetAliasDefaultLabelRequest | onFetch: got " + v )

          val sessionURIStr = msg.sessionURI.toString

          v match {
            case PostedExpr( defaultLabel : CnxnCtxtLabel[String,String,String] ) => {
              CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
                ( "msgType" -> "getAliasDefaultLabelResponse" ) ~
                ( "content" ->
                  ( "sessionURI" -> sessionURIStr ) ~
                  ( "label" -> defaultLabel.toString )
                )
              ) ) )
            }
            case Bottom => {
              CometActorMapper.cometMessage( key, sessionURIStr, compact( render(
                ( "msgType" -> "getAliasDefaultLabelError" ) ~
                ( "content" ->
                  ( "sessionURI" -> sessionURIStr ) ~
                  ( "reason" -> "No default label exists" )
                )
              ) ) )
            }
          }
        }
      }
    }

    agentMgr().fetch( defaultLabelStorageLocation, List( aliasStorageCnxn ), onFetch )
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
