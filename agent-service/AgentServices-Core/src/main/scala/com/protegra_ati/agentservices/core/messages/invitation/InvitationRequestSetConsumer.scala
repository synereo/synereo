package com.protegra_ati.agentservices.core.messages.invitation

import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.ResourceExtensions._
import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util._
import com.protegra_ati.agentservices.core.schema._
//import com.protegra.i18n.ResourceManager
//import com.protegra.config.ConfigurationManager
import scala.concurrent.ops._
import scala.collection.JavaConversions._
import java.util.{Locale, UUID, ResourceBundle, Date}
import java.text.DateFormat
import org.apache.commons.lang3.text.StrSubstitutor
//import com.protegra_ati.agentservices.core.platformagents.behaviors.notification.Email
import com.protegra_ati.agentservices.core.util.serializer.Serializer

trait InvitationRequestSetConsumer
{
  self: AgentHostStorePlatformAgent =>

  def listenPublicInvitationConsumerRequests(cnxn: AgentCnxnProxy) =
  {
    listen(_publicQ, cnxn, Channel.Invitation, Some(ChannelRole.Consumer), ChannelType.Request, ChannelLevel.Public, handlePublicInvitationConsumerRequestChannel(_: AgentCnxnProxy, _: Message))
  }

  protected def handlePublicInvitationConsumerRequestChannel(cnxn: AgentCnxnProxy, msg: Message) =
  {
    //these are request coming on the public channel (from us or other PAs)
    //if we get in this handler, it means the message was meant for us and we should process it
    report("entering handlePublicInvitationConsumerRequestChannel in ConnectionBroker", Severity.Trace)

    msg match {

      case x: InvitationRequest => {
        processInvitationRequest(cnxn, x)
      }


      case _ => report("***********************not doing anything in handlePublicInvitationConsumerRequestChannel", Severity.Error)
    }
    report("exiting handlePublicInvitationConsumerRequestChannel in ConnectionBroker", Severity.Trace)
  }

  protected def processInvitationRequest(cnxnA_Broker: AgentCnxnProxy, inviteRequest: InvitationRequest) =
  {

    println("----------------------------------------------->>>> cnxnA_Broker= cnxnA_Broker.scr" + cnxnA_Broker.src + ", cnxnA_Broker.trgt=" + cnxnA_Broker.trgt + ", inviteRequest=" + inviteRequest)
    val agentIsInvitationInitiator = isCaptured(cnxnA_Broker, inviteRequest)
    //    processInvitationRequestOnPAOnBehalfOfInitiatorAgent(cnxnA_Broker, inviteRequest)

    //TODO: temporary hack until agentservices-ati is merged for the double get
    // store autoacepts invite request on behalf of the initiator
    if ( agentIsInvitationInitiator ) {
      processInvitationRequestOnPAOnBehalfOfInitiatorAgent(cnxnA_Broker, inviteRequest)
    }
    // agent is the target of the invitation
    else {
      processInvitationRequestOnPAOnBehalfOfRequestedAgent(cnxnA_Broker, inviteRequest)
    }

  }

  protected def processInvitationRequestOnPAOnBehalfOfInitiatorAgent(cnxnA_Broker: AgentCnxnProxy, inviteRequest: InvitationRequest) =
  {
    report("****Post invite request, auto processing on behalf of initiator : " + cnxnA_Broker.trgt)
    var requestedCategory = ConnectionCategory.Person.toString
    inviteRequest.requestedCategory match {
      case None => {
        report("Missing category in the InvitationRequest " + inviteRequest, Severity.Error)
      }
      case Some(category) => {
        requestedCategory = category
      }
    }

    // TODO basic, full, etc. into constants pool
    var requestedConnectionType = "Basic"
    inviteRequest.requestedConnectionType match {
      case None => {
        report("Missing connectionType in the InvitationRequest " + inviteRequest, Severity.Error)
      }
      case Some(connectionType) => {
        requestedConnectionType = connectionType
      }
    }

    var requestedConnectionName = "New Connection"
    inviteRequest.requestedConnectionName match {
      case None => {
        report("Missing connectionName in the InvitationRequest " + inviteRequest, Severity.Error)
      }
      case Some(x) => {
        requestedConnectionName = x
      }
    }

    val response = new InvitationResponse(inviteRequest.ids.copyAsChild(), inviteRequest.eventKey, requestedCategory, requestedConnectionType, requestedConnectionName, null, inviteRequest.conversationThread, true)
    response.targetCnxn = inviteRequest.targetCnxn
    response.originCnxn = inviteRequest.originCnxn

    //TODO: jsk - temporary - too close in unit test, causing db concurrency issues?
    Thread.sleep(500)
    report("****Post invite request, sending the accepted response: " + response.getChannelKey + "****", Severity.Info)
    singleSend(_publicQ, response.targetCnxn, response)

  }

  protected def processInvitationRequestOnPAOnBehalfOfRequestedAgent(cnxnA_Broker: AgentCnxnProxy, inviteRequest: InvitationRequest) =
  {
    println("I'm NOT an initiator: " + cnxnA_Broker.trgt + " just someone wants to be connected to me")
    //lookup the self connection from the systemdata in the connection silo
    val queryObject = new SystemData(new Connection())
    fetch[ SystemData[ Connection ] ](_dbQ, cnxnA_Broker, queryObject.toSearchKey, handleSystemDataLookupStoreInvitationRequest(_: AgentCnxnProxy, _: SystemData[ Connection ], inviteRequest))
  }

  protected def handleSystemDataLookupStoreInvitationRequest(cnxn: AgentCnxnProxy, systemConnection: SystemData[ Connection ], inviteRequest: InvitationRequest): Unit =
  {
    report("STORE INVITATIONS FOR LATER RESPONSE: inviteRequest=" + inviteRequest + ", cnxn=" + cnxn, Severity.Info)
    val selfConnection = systemConnection.data
    val persistedInvitationRequestMessage = new PersistedMessage[ InvitationRequest ](inviteRequest)
    store(_dbQ, selfConnection.writeCnxn, persistedInvitationRequestMessage.toStoreKey, Serializer.serialize[ PersistedMessage[ InvitationRequest ] ](persistedInvitationRequestMessage))
    val profileQuery = new Profile()
    fetch[ Data ](_dbQ, selfConnection.writeCnxn, profileQuery.toSearchKey, notificationHandler(_: AgentCnxnProxy, _: Data, inviteRequest))

  }

  protected def notificationHandler(cnxn: AgentCnxnProxy, user: Data, inviteRequest: InvitationRequest) =
  {
//    user match {
//      case p: Profile => {
//        try {
//          val locale: Locale = new Locale(p.localeCode)
//          val resourceBundle = ResourceManager.getResourceBundle(locale)
//          val templateResourceBundle = ResourceManager.getTemplateResourceBundle(locale)
//          val messageTemplate = templateResourceBundle.getString("connection_request_notification_email")
//          val formatter: DateFormat = DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG, locale);
//          // replacement logic for variables in a template
//          var valuesMap: Map[ String, String ] = Map[ String, String ]()
//          valuesMap += ( "TIMESTAMP" -> formatter.format(new Date) )
//          valuesMap += ( "USER" -> p.firstName )
//          valuesMap += ( "CONNECTION_REQUEST_URL" -> ConfigurationManager.getConfigurationManager().getString("notification.email.connection.request.url") )
//          valuesMap += ( "CONNECTION_REQUEST_UID" -> inviteRequest.ids.conversationId )
//          val sub: StrSubstitutor = new StrSubstitutor(valuesMap)
//          val message: String = sub.replace(messageTemplate);
//          sendNotification(Email.createSystemEmail(p, resourceBundle.getString("connection.request.notification.email.system.subject"), message, "text/html"))
//          report("send notification about connection request to " + p.firstName + " " + p.lastName, Severity.Trace)
//        } catch {
//          case e: Exception => {
//            report("unable to send notification due to " + e.getCause.toString, Severity.Error)
//            e.printStackTrace()
//          }
//          case _ => {}
//        }
//      }
//      case _ => {}
//    }
  }
}