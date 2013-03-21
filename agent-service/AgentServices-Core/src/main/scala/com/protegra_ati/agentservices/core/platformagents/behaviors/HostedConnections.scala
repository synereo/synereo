package com.protegra_ati.agentservices.core.platformagents.behaviors

/* User: jklassen
*/

import com.protegra_ati.agentservices.core.platformagents._
import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.store.util.Severity
import com.protegra_ati.agentservices.core.schema.util._

trait HostedConnections
{
  self: AgentHostStorePlatformAgent =>
  // TODO this list has to be extended every time new self connection is created (in a registration process)
 @volatile var _cnxnUserSelfConnectionsList = List[ AgentCnxnProxy ]()

  def loadUserCnxnList()
  {
    val userCnxns = Connection.SEARCH_ALL
    fetch[ Connection ](_dbQ, _storeCnxn, userCnxns.toSearchKey, handleUserCnxnsFetchOnStartup(_: AgentCnxnProxy, _: Connection))
  }

  protected def handleUserCnxnsFetchOnStartup(storeCnxn: AgentCnxnProxy, userConnection: Connection)
  {
    _cnxnUserSelfConnectionsList = List[ AgentCnxnProxy ](userConnection.writeCnxn) ::: _cnxnUserSelfConnectionsList
  }

  def listenForHostedCnxns() =
  {
    for ( cnxn <- _cnxnUserSelfConnectionsList ) {
      listenForHostedCnxn(cnxn)
    }
  }

  def addToHostedCnxn(selfCnxn: AgentCnxnProxy) =
  {
    if ( _cnxnUserSelfConnectionsList.contains(selfCnxn) ) {/*do nothing already exists*/}
    else _cnxnUserSelfConnectionsList = selfCnxn :: _cnxnUserSelfConnectionsList
  }

  //used in AgentHostCombinedStoreUIPlatformAgentTest
  def listenForHostedCnxn(selfCnxn: AgentCnxnProxy) =
  {
    //these are user self connections, doesn't matter whether we use
    //the read or write connection
    listenPublicRequests(selfCnxn)
    listenPublicResponses(selfCnxn)

    //for each other these connections, listen for all the connection.writeCnxn
    //in each of their silos
    val search = Connection.SEARCH_ALL
    fetch[ Connection ](_dbQ, selfCnxn, search.toSearchKey, handleUserConnectionPostFetch(_: AgentCnxnProxy, _: Connection))
  }

  // If Jen is on PA
  // on publicQ listen for all requests on JenMike
  // on publicQ listen for all responses on MikeJen
  protected def handleUserConnectionPostFetch(cnxn: AgentCnxnProxy, userConnection: Connection) =
  {
    listenPublicRequests(userConnection.writeCnxn)
    listenPublicResponses(userConnection.readCnxn)
    //this is on UI
  }

  //  deprecated from store
  //  def handleUserConnectionPostFetch(cnxn: AgentCnxnProxy, userConnection: Connection) =
  //  {
  //    listenPublicRequests(userConnection.readCnxn)
  //    listenPublicResponses(userConnection.readCnxn)
  //  }

}