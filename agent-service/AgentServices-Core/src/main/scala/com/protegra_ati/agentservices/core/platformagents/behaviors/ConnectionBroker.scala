package com.protegra_ati.agentservices.core.platformagents.behaviors

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.core.messages._
import com.protegra.agentservicesstore.usage.AgentKVDBScope._
import com.protegra.agentservicesstore.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import org.joda.time.{DateTime, Instant}
import java.util.{UUID, HashMap}
import com.protegra_ati.agentservices.core.schema.util._
import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.util.serializer.Serializer
import com.protegra.agentservicesstore.util.Severity


trait ConnectionBroker
{
  self: AgentHostStorePlatformAgent =>

  //  def handleBrokerTaskForNewConnection(selfCnxn:AgentCnxnProxy, newConnection:Connection)
  //  {
  //    //if the self (agentCnxn) contains a broker token, then it is a broker connection
  //    //and we want to generate Introduction objects on the connection.writeCnxn silo
  //    //so the other side can view and interact with those objects
  //    val queryObject = new Search[BrokerToken](classOf[BrokerToken])
  //    fetch[BrokerToken](_dbQ, selfCnxn, queryObject.toSearchKey, handlePostBrokerTokenFetch(_: AgentCnxnProxy, _: BrokerToken, newConnection))
  //  }
  //
  //  def handlePostBrokerTokenFetch(selfCnxn: AgentCnxnProxy, brokerToken: BrokerToken, newConnection:Connection) =
  //  {
  //    //there is a token so for connection, generate connectionintro objects
  //    //for each connection in the brokers self silo....that is the design pattern
  //    //for now we are simply storing 4 hard coded ones....
  //    //need to check for known connections (the ones below) and not process them
  //    //as we are just reseeding the databases in that case
  //
  //    val jenId = UUID.fromString("dbd56858-adfe-4a12-a22c-f356dff4508d")
  //    val mikeId = UUID.fromString("e80b9b6f-0beb-4a33-b9a4-8b14d51461bd")
  //    val jasonId = UUID.fromString("fd408800-28c1-4854-8555-82123a48dde3")
  //    val jasonId = UUID.fromString("f671e858-8370-4195-97b7-3f2a3631bf55")
  //
  //    if ( newConnection.id != jenId
  //      && newConnection.id != mikeId
  //      && newConnection.id != jasonId
  //      && newConnection.id != jasonId ) {
  //
  //      val jensConnectionIntro = new Introduction(UUID.randomUUID(), "Jen", jenId, newConnection.id)
  //      updateDataById(newConnection.writeCnxn, jensConnectionIntro)
  //      val mikesConnectionIntro = new Introduction(UUID.randomUUID(), "Mike", mikeId, newConnection.id)
  //      updateDataById(newConnection.writeCnxn, mikesConnectionIntro)
  //      val jasonsConnectionIntro = new Introduction(UUID.randomUUID(), "Jason", jasonId, newConnection.id)
  //      updateDataById(newConnection.writeCnxn, jasonsConnectionIntro)
  //      val colinsConnectionIntro = new Introduction(UUID.randomUUID(), "Colin", colinId, newConnection.id)
  //      updateDataById(newConnection.writeCnxn, colinsConnectionIntro)
  //    }
  //
  //  }
  //
  //  protected def generateIntroduction(targetAlias: String, brokerCnxn:AgentCnxnProxy) = {
  //    val oldData: Connection = null
  //    val intro = new Introduction(UUID.randomUUID(), targetAlias, )
  //    updateDataById(connection.writeCnxn, intro)
  //  }

  def generateSystemData(selfCnxn: AgentCnxnProxy, connection: Connection): SystemData[ Connection ] =
  {
    //we need to store a SystemData object here at this time as well which contains the self cnxn's for both parties
    //of the connection....the cnxn passed in is one of the self connections, we need a way to get the other....
    //faking it out for now
    //the lines below needs to be replaced by a new message that can go across the publicQ to the right PA and
    //retrieve the "read" side's self connection
//    val targetSelfCnxn = AgentCnxnProxy(connection.writeCnxn.src, "", connection.writeCnxn.src)
    val selfCnxns = new Connection(ConnectionCategory.Self.toString, "Full", "System", selfCnxn, selfCnxn, "false", List[ String ]())

    val systemConnection = new SystemData[ Connection ](selfCnxns)
    store(_dbQ, connection.writeCnxn, systemConnection.toStoreKey, Serializer.serialize[ SystemData[ Connection ] ](systemConnection))
    systemConnection
  }

  def generateCacheData(selfCnxn: AgentCnxnProxy, newConnection: Connection): Unit =
  {
    report("generateCacheData not implemented", Severity.Trace)
  }

}