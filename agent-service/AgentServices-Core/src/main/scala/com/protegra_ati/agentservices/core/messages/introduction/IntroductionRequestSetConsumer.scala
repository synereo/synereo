package com.protegra_ati.agentservices.core.messages.introduction

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.util._
import com.protegra_ati.agentservices.core.schema._
import content._
import com.protegra_ati.agentservices.core.util.serializer.Serializer


trait IntroductionRequestSetConsumer
{
  self: AgentHostStorePlatformAgent =>

  def listenPublicIntroductionConsumerRequests(cnxn: AgentCnxn) =
  {
    listen(_publicQ, cnxn, Channel.Introduction, Some(ChannelRole.Consumer), ChannelType.Request, ChannelLevel.Public, handlePublicIntroductionConsumerRequestChannel(_: AgentCnxn, _: Message))
  }

  protected def handlePublicIntroductionConsumerRequestChannel(cnxn: AgentCnxn, msg: Message) =
  {
    //these are request coming on the public channel (from us or other PAs)
    //if we get in this handler, it means the message was meant for us and we should process it
    report("entering handlePublicIntroductionConsumerRequestChannel in ConnectionBroker", Severity.Trace)

    msg match {

      case x: IntroductionRequest => {
        //TODO: not sure if this processing is correct
        processIntroductionRequest(cnxn, x)
      }

      case _ => report("***********************not doing anything in handlePublicIntroductionConsumerRequestChannel", Severity.Error)
    }
    report("exiting handlePublicIntroductionConsumerRequestChannel in ConnectionBroker", Severity.Trace)
  }

  protected def processIntroductionRequest(cnxnA_Broker: AgentCnxn, introRequest: IntroductionRequest) =
  {
    println("----------------------------------------------->>>> cnxnA_Broker= cnxnA_Broker.scr" + cnxnA_Broker.src + ", cnxnA_Broker.trgt=" + cnxnA_Broker.trgt + ", introRequest=" + introRequest)
//    val agentIsIntroductionInitiator = isCaptured(cnxnA_Broker, introRequest) //only needed for invite
//    println("I'm NOT an initiator: " + cnxnA_Broker.trgt + " just someone wants to be connected to me")
    //lookup the self connection from the systemdata in the connection silo
    val queryObject = new SystemData(new Connection())
    fetch[ SystemData[ Connection ] ](_dbQ, cnxnA_Broker, queryObject.toSearchKey, handleSystemDataLookupStoreIntroductionRequest(_: AgentCnxn, _: SystemData[ Connection ], introRequest))
  }

  protected def handleSystemDataLookupStoreIntroductionRequest(cnxn: AgentCnxn, systemConnection: SystemData[ Connection ], introRequest: IntroductionRequest): Unit =
  {
    println("$$$$$$$$$$$ STORE INTRODUCTIONS FOR LATER RESPONSE, cnxn=" + cnxn + ", systemConnection=" + systemConnection + ", introRequest=" + introRequest)
    val selfConnection = systemConnection.data
    val persistedIntroductionRequestMessage = new PersistedMessage[ IntroductionRequest ](introRequest)
    store(_dbQ, selfConnection.writeCnxn, persistedIntroductionRequestMessage.toStoreKey, Serializer.serialize[ PersistedMessage[ IntroductionRequest ] ](persistedIntroductionRequestMessage))
  }


}