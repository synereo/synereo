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

  var _externalIdsStorageLocation : Option[CnxnCtxtLabel[String,String,String]] = None
  def externalIdsStorageLocation() : CnxnCtxtLabel[String,String,String] = {
    _externalIdsStorageLocation match {
      case Some( csl ) => csl
      case None => {
        fromTermString(
          "externalIdsList( true )"
        ).getOrElse(
          throw new Exception( "Couldn't parse label: " + "externalIdsList( true )" )
        )
      }
    }
  }

  var _defaultExternalIdStorageLocation : Option[CnxnCtxtLabel[String,String,String]] = None
  def defaultExternalIdStorageLocation() : CnxnCtxtLabel[String,String,String] = {
    _defaultExternalIdStorageLocation match {
      case Some( csl ) => csl
      case None => {
        fromTermString(
          "defaultExternalId( true )"
        ).getOrElse(
          throw new Exception( "Couldn't parse label: " + "defaultExternalId( true )" )
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

  var _biCnxnsStorageLocation : Option[CnxnCtxtLabel[String,String,String]] = None
  def biCnxnsStorageLocation() : CnxnCtxtLabel[String,String,String] = {
    _biCnxnsStorageLocation match {
      case Some( csl ) => csl
      case None => {
        fromTermString(
          "biCnxnsList( true )"
        ).getOrElse(
          throw new Exception( "Couldn't parse label: " + "biCnxnsList( true )" )
        )
      }
    }
  }

  def agentFromSession(
    sessionURI: URI
  ) : URI = {
    new URI(
      "agent",
      sessionURI.getHost(),
      null,
      null
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
    msg : createAgentRequest
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handlecreateAgentRequest with msg : " + msg
    )
  }
  //    - `authType == "password"` (case-insensitive)
  
  //### initializeSession
  def handleinitializeSessionRequest(
    msg : initializeSessionRequest
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleinitializeSessionRequest with msg : " + msg
    )
  }
  
  //### External identities
  //#### addAgentExternalIdentity
  def handleaddAgentExternalIdentityRequest[ID](
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
    msg : addAgentExternalIdentityToken
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleaddAgentExternalIdentityToken with msg : " + msg
    )
  }
  
  
  //#### removeAgentExternalIdentities
  def handleremoveAgentExternalIdentitiesRequest[ID](
    msg : removeAgentExternalIdentitiesRequest[ID]
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleremoveAgentExternalIdentitiesRequest with msg : " + msg
    )
  }
  
  //#### getAgentExternalIdentities
  def handlegetAgentExternalIdentitiesRequest[IDType](
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

            CometActorMapper.cometMessage(sessionURIStr, compact( render(
              ( "msgType" -> "addAgentAliasesResponse" ) ~
              ( "content" -> ( "sessionURI" -> sessionURIStr ) )
            ) ) )
          }

          v match {              
            case PostedExpr( (PostedExpr( previousAliasList : String ), _, _) ) => {
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
          CometActorMapper.cometMessage(sessionURIStr, compact( render(
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

            CometActorMapper.cometMessage(sessionURIStr, compact( render(
              ( "msgType" -> "removeAgentAliasesResponse" ) ~
              ( "content" -> ( "sessionURI" -> sessionURIStr ) )
            ) ) )
          }

          v match {
            case PostedExpr( (PostedExpr( previousAliasList : List[String] ), _, _) ) => {
              val newAliasList = previousAliasList.filterNot( msg.aliases.contains )

              BasicLogService.tweet(
                "handleremoveAgentAliasesRequest | onGet | onPut | updating aliasList with " + newAliasList
              )

              agentMgr().put[List[String]]( aliasStorageLocation, List( aliasStorageCnxn ), newAliasList, onPut )
            }
            case Bottom => {
              BasicLogService.tweet( "handleremoveAgentAliasesRequest | onGet: no aliasList exists" )

              CometActorMapper.cometMessage(sessionURIStr, compact( render(
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
            case PostedExpr( (PostedExpr( aliasList : List[String] ), _, _) ) => aliasList
            case Bottom => Nil
          }

          CometActorMapper.cometMessage(sessionURIStr, compact( render(
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
              case PostedExpr( (PostedExpr( defaultAlias : String ), _, _) ) => {
                CometActorMapper.cometMessage(sessionURIStr, compact( render(
                  ( "msgType" -> "getDefaultAliasResponse" ) ~
                  ( "content" ->
                    ( "sessionURI" -> sessionURIStr ) ~
                    ( "alias" -> defaultAlias )
                  )
                ) ) )
              }
              case Bottom => {
                CometActorMapper.cometMessage(sessionURIStr, compact( render(
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
    msg : setDefaultAliasRequest
  ) : Unit = {
    BasicLogService.tweet( "Entering: handlesetDefaultAliasRequest with msg : " + msg )

    val aliasStorageCnxn = identityAliasFromAgent( agentFromSession( msg.sessionURI ) )

    val onPut : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      BasicLogService.tweet( "handlesetDefaultAliasRequest | onPut" )

      val sessionURIStr = msg.sessionURI.toString

      CometActorMapper.cometMessage(sessionURIStr, compact( render(
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
    msg : addAliasExternalIdentitiesRequest[ID]
  ) : Unit = {
    BasicLogService.tweet( "Entering: handleaddAliasExternalIdentitiesRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )

    val onGet : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      optRsrc match {
        case None => {
          // Nothing to be done
          BasicLogService.tweet( "handleaddAliasExternalIdentitiesRequest | onGet: got None" )
        }
        case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
          BasicLogService.tweet( "handleaddAliasExternalIdentitiesRequest | onGet: got " + v )

          val onPut : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
            BasicLogService.tweet( "handleaddAliasExternalIdentitiesRequest | onGet | onPut" )

            val sessionURIStr = msg.sessionURI.toString

            CometActorMapper.cometMessage(sessionURIStr, compact( render(
              ( "msgType" -> "addAliasExternalIdentitiesResponse" ) ~
              ( "content" -> ( "sessionURI" -> sessionURIStr ) )
            ) ) )
          }

          v match {
            case PostedExpr( (PostedExpr( previousExternalIdsList : List[ID] ), _, _) ) => {
              val newExternalIdsList = previousExternalIdsList ++ msg.ids

              BasicLogService.tweet(
                "handleaddAliasExternalIdentitiesRequest | onGet | onPut | updating externalIdsList with " + newExternalIdsList
              )

              agentMgr().put[List[ID]]( externalIdsStorageLocation, List( aliasStorageCnxn ), newExternalIdsList, onPut )
            }
            case Bottom => {
              agentMgr().put[List[ID]]( externalIdsStorageLocation, List( aliasStorageCnxn ), msg.ids, onPut )
            }
          }
        }
      }
    }

    agentMgr().get( externalIdsStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  //    - Only ids already on the agent are allowed
  
  
  //#### removeAliasExternalIdentities
  def handleremoveAliasExternalIdentitiesRequest[ID](
    msg : removeAliasExternalIdentitiesRequest[ID]
  ) : Unit = {
    BasicLogService.tweet( "Entering: handleremoveAliasExternalIdentitiesRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )

    val onGet : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      optRsrc match {
        case None => {
          // Nothing to be done
          BasicLogService.tweet( "handleremoveAliasExternalIdentitiesRequest | onGet: got None" )
        }
        case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
          BasicLogService.tweet( "handleremoveAliasExternalIdentitiesRequest | onGet: got " + v )

          val sessionURIStr = msg.sessionURI.toString

          val onPut : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
            BasicLogService.tweet( "handleremoveAliasExternalIdentitiesRequest | onGet | onPut" )

            CometActorMapper.cometMessage(sessionURIStr, compact( render(
              ( "msgType" -> "removeAliasExternalIdentitiesResponse" ) ~
              ( "content" -> ( "sessionURI" -> sessionURIStr ) )
            ) ) )
          }

          v match {
            case PostedExpr( (PostedExpr( previousExternalIdsList : List[ID] ), _, _) ) => {
              val newExternalIdsList = previousExternalIdsList.filterNot( msg.ids.contains )

              BasicLogService.tweet(
                "handleremoveAliasExternalIdentitiesRequest | onGet | onPut | updating cnxnList with " + newExternalIdsList
              )

              agentMgr().put[List[ID]]( externalIdsStorageLocation, List( aliasStorageCnxn ), newExternalIdsList, onPut )
            }
            case Bottom => {
              BasicLogService.tweet( "handleremoveAliasExternalIdentitiesRequest | onGet: no externalIdsList exists" )

              CometActorMapper.cometMessage(sessionURIStr, compact( render(
                ( "msgType" -> "removeAliasExternalIdentitiesError" ) ~
                ( "content" ->
                  ( "sessionURI" -> sessionURIStr ) ~
                  ( "reason" -> "no externalIdsList exists" )
                )
              ) ) )
            }
          }
        }
      }
    }

    agentMgr().get( externalIdsStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  
  
  //#### getAliasExternalIdentities
  def handlegetAliasExternalIdentitiesRequest[IDType](
    msg : getAliasExternalIdentitiesRequest[IDType]
  ) : Unit = {
    BasicLogService.tweet( "Entering: handlegetAliasExternalIdentitiesRequest with msg : " + msg )
  }
  //    - One value of `IDType` is `ANY`
  
  //#### setAliasDefaultExternalIdentity
  def handlesetAliasDefaultExternalIdentityRequest[ID](
    msg : setAliasDefaultExternalIdentityRequest[ID]
  ) : Unit = {
    BasicLogService.tweet( "Entering: handlesetAliasDefaultExternalIdentityRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )

    val onPut : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      BasicLogService.tweet( "handlesetAliasDefaultExternalIdentityRequest | onPut" )

      val sessionURIStr = msg.sessionURI.toString

      CometActorMapper.cometMessage(sessionURIStr, compact( render(
        ( "msgType" -> "setAliasDefaultExternalIdentityResponse" ) ~
        ( "content" -> ( "sessionURI" -> sessionURIStr ) )
      ) ) )
    }

    agentMgr().put[ID]( defaultExternalIdStorageLocation, List( aliasStorageCnxn ), msg.id, onPut )
  }
  
  
  //### Connections
  //#### removeAliasConnections
  def handleremoveAliasConnectionsRequest(
    msg : removeAliasConnectionsRequest
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

            // TODO: Should the write cnxn be cleared out or removed?

            CometActorMapper.cometMessage(sessionURIStr, compact( render(
              ( "msgType" -> "removeAliasConnectionsResponse" ) ~
              ( "content" -> ( "sessionURI" -> sessionURIStr ) )
            ) ) )
          }

          v match {
            case PostedExpr( (PostedExpr( previousBiCnxnListStr : String ), _, _) ) => {
              // TODO: Deserialize string
              // TODO: Filter BiCnxns based on passed in Cnxns
              // TODO: Serialize new BiCnxns list
              val newBiCnxnListStr = previousBiCnxnListStr

              BasicLogService.tweet(
                "handleremoveAliasConnectionsRequest | onGet | onPut | updating biCnxnList with " + newBiCnxnListStr
              )

              agentMgr().put( biCnxnsStorageLocation, List( aliasStorageCnxn ), newBiCnxnListStr, onPut )
            }
          }
        }
      }
    }

    agentMgr().get( biCnxnsStorageLocation, List( aliasStorageCnxn ), onGet )
  }

  //#### getAliasConnections
  def handlegetAliasConnectionsRequest(
    msg : getAliasConnectionsRequest
  ) : Unit = {
    BasicLogService.tweet( "Entering: handlegetAliasConnectionsRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )

    val onFetch : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      optRsrc match {
        case None => {
          // Nothing to be done
          BasicLogService.tweet( "handlegetAliasConnectionsRequest | onFetch: got None" )
        }
        case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
          BasicLogService.tweet( "handlegetAliasConnectionsRequest | onFetch: got " + v )

          val sessionURIStr = msg.sessionURI.toString

          val biCnxnList = v match {
            case PostedExpr( (PostedExpr( biCnxnListStr : String ), _, _) ) => {
              Serializer.deserialize[List[PortableAgentBiCnxn]]( biCnxnListStr )
            }
            case Bottom => Nil
          }

          def biCnxnToJObject( biCnxn : PortableAgentBiCnxn ) : JObject = {
            ( "source" -> biCnxn.writeCnxn.src.toString ) ~
            ( "label" -> biCnxn.writeCnxn.label ) ~
            ( "target" -> biCnxn.writeCnxn.trgt.toString )
          }

          CometActorMapper.cometMessage(sessionURIStr, compact( render(
            ( "msgType" -> "getAliasConnectionsResponse" ) ~
            ( "content" ->
              ( "sessionURI" -> sessionURIStr ) ~
              ( "connections" -> biCnxnList.map( biCnxnToJObject( _ ) ) )
            )
          ) ) )
        }
      }
    }

    agentMgr().fetch( biCnxnsStorageLocation, List( aliasStorageCnxn ), onFetch )
  }

  //### Labels
  //#### addAliasLabels
  def handleaddAliasLabelsRequest(
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

            CometActorMapper.cometMessage(sessionURIStr, compact( render(
              ( "msgType" -> "addAliasLabelsResponse" ) ~
              ( "content" -> ( "sessionURI" -> sessionURIStr ) )
            ) ) )
          }

          v match {
            case PostedExpr( (PostedExpr( previousLabelList : String ), _, _) ) => {
              val newLabelList = compact(render(parse(previousLabelList) ++ msg.labels.map(_.toString.replace("'",""))))
              BasicLogService.tweet("handleaddAliasLabelsRequest | onGet | onPut | updating labelList with " + newLabelList )
              agentMgr().put[String](
                labelsStorageLocation,
                List( aliasStorageCnxn ),
                newLabelList,
                onPut
              )
            }
            case Bottom => {
              agentMgr().put[String](
                labelsStorageLocation,
                List( aliasStorageCnxn ),
                compact(render(msg.labels.map(_.toString.replace("'","")))),
                onPut
              )
            }
          }
        }        
        case wonky => {
          CometActorMapper.cometMessage(sessionURIStr, compact( render(
            ( "msgType" -> "addAliasLabelsError" ) ~ 
            ( "content" -> ( "reason" -> ("Got wonky response: " + wonky) ) )
          ) ) )
        }
      }
    }
    
    agentMgr().get( labelsStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  //    - `Label = String`
  
  
  //#### updateAliasLabels
  def handleupdateAliasLabelsRequest(
    msg : updateAliasLabelsRequest
  ) : Unit = {
    BasicLogService.tweet(
      "Entering: handleupdateAliasLabelsRequest with msg : " + msg
    )
    val aliasStorageCnxn =
      getAliasCnxn( msg.sessionURI, msg.alias )
    val onGet : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      optRsrc match {
        case None => {
          // Nothing to be done
          BasicLogService.tweet(
            "handleupdateAliasLabelsRequest | onGet: got None"
          )
        }
        case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
          BasicLogService.tweet(
            "handleupdateAliasLabelsRequest | onGet: got " + v
          )
          val onPut : Option[mTT.Resource] => Unit =
            ( optRsrc : Option[mTT.Resource] ) => {
              BasicLogService.tweet("handleupdateAliasLabelsRequest | onGet | onPut")
              CometActorMapper.cometMessage(
                msg.sessionURI.toString,
                compact(
                  render(
                    ( "msgType" -> "updateAliasLabelsResponse" ) ~ ( "content" -> ( "sessionURI" -> msg.sessionURI.toString ) )
                  )
                )
              )
            }
          BasicLogService.tweet("handleupdateAliasLabelsRequest | onGet | onPut | updating labelList with " + 
            compact(render(msg.labels.map(_.toString.replace("'",""))))
          )
          agentMgr().put[String](
            labelsStorageLocation,
            List( aliasStorageCnxn ),
            compact(render(msg.labels.map(_.toString.replace("'","")))),
            onPut
          )
        }
      }
    }

    agentMgr().get( labelsStorageLocation, List( aliasStorageCnxn ), onGet )
  }
  
  
  //#### getAliasLabels
  def handlegetAliasLabelsRequest(
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

          val labelList: JArray = v match {
            case PostedExpr( (PostedExpr( labelList : String ), _, _) ) =>
              parse(labelList).asInstanceOf[JArray]
            case Bottom => Nil
          }

          CometActorMapper.cometMessage(sessionURIStr, compact( render(
            ( "msgType" -> "getAliasLabelsResponse" ) ~
            ( "content" ->
              ( "sessionURI" -> sessionURIStr ) ~
              ( "labels" -> labelList )
            )
          ) ) )
        }
      }
    }

    agentMgr().fetch( labelsStorageLocation, List( aliasStorageCnxn ), onFetch )
  }
  
  //#### setAliasDefaultLabel
  def handlesetAliasDefaultLabelRequest(
    msg : setAliasDefaultLabelRequest
  ) : Unit = {
    BasicLogService.tweet( "Entering: handlesetAliasDefaultLabelRequest with msg : " + msg )

    val aliasStorageCnxn = getAliasCnxn( msg.sessionURI, msg.alias )

    val onPut : Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      BasicLogService.tweet( "handlesetAliasDefaultLabelRequest | onPut" )

      val sessionURIStr = msg.sessionURI.toString

      CometActorMapper.cometMessage(sessionURIStr, compact( render(
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
            case PostedExpr( (PostedExpr( defaultLabel : CnxnCtxtLabel[String,String,String] ), _, _) ) => {
              CometActorMapper.cometMessage(sessionURIStr, compact( render(
                ( "msgType" -> "getAliasDefaultLabelResponse" ) ~
                ( "content" ->
                  ( "sessionURI" -> sessionURIStr ) ~
                  ( "label" -> defaultLabel.toString )
                )
              ) ) )
            }
            case Bottom => {
              CometActorMapper.cometMessage(sessionURIStr, compact( render(
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
    msg : evalSubscribeCancelRequest
  ) : Unit = {
    BasicLogService.tweet( 
      "Entering: handleevalSubscribeCancelRequest with msg : " + msg
    )
    val onCancel: Option[mTT.Resource] => Unit = ( optRsrc : Option[mTT.Resource] ) => {
      optRsrc match {
        case None => {
          // Nothing to be done
          BasicLogService.tweet( "handleevalSubscribeCancelRequest | onFetch: got None" )
        }
        case _ => {
          CometActorMapper.cometMessage(msg.sessionURI.toString, compact(render(
            ("msgType" -> "evalSubscribeCancelResponse") ~
            ("content" -> ("sessionURI" -> msg.sessionURI.toString))
          )))
        }
      }
    }
    for (filter <- msg.filter) {
      agentMgr().cancel(filter, msg.connections, onCancel)
    }
  }
}
