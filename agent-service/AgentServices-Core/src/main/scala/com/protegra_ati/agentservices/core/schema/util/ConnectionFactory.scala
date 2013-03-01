package com.protegra_ati.agentservices.core.schema.util

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.protegra_ati.agentservices.store.usage.AgentKVDBScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.UUID
import java.net.URI
import com.protegra_ati.agentservices.core.schema._
import scala.collection.JavaConversions._

object ConnectionFactory
{
  def createSelfConnection(alias: String, id: String): Connection =
  {
    createConnection(alias, ConnectionCategory.Self.toString, ConnectionCategory.Self.toString, "Trusted", id, id)
  }

  def createConnection(alias: String, categorySource: String, categoryTarget: String, connectionType: String, sourceId: String, targetId: String): Connection =
  {
    createConnection(alias, categoryTarget, connectionType, sourceId, targetId, "true", getPolicies(categorySource,categoryTarget))
  }

  /**
   * to be used for search for all connections, all fields are empty strings, policies object is Nil
   * @return empty connection for search
   */
  def createConnection(): Connection =
  {
    createConnection("", "", "", "", "", "", Nil)
  }


  /**
   * to be used for search for all connections with an specified connectionType
   * @param connectionType
   * @return connection with only one parameter "connectionType" for search
   */
  def createTypedConnection(connectionType: String): Connection =
  {
    new Connection("", connectionType, "", null, null, "", Nil)
  }


//  def createConnection(alias: String, categorySource: String, categoryTarget: String, connectionType: String, sourceId: String, targetId: String, autoApprove: String): Connection =
//  {
//    createConnection(alias, categoryTarget, connectionType, sourceId, targetId, autoApprove, getPolicies(categorySource,categoryTarget))
//  }

  private def createConnection(alias: String,
    category: String,
    connectionType: String,
    sourceId: String,
    targetId: String,
    autoApprove: String,
    policies: List[ String ]): Connection =
  {
    val readCnxn = new AgentCnxnProxy(targetId.toURI, "", sourceId.toURI)
    val writeCnxn = new AgentCnxnProxy(sourceId.toURI, "", targetId.toURI)
    new Connection(category, connectionType, alias, readCnxn, writeCnxn, autoApprove, policies)
    //adding the generation of the
    // TODO eventually validation should here take place
  }

  def getPolicies(categorySource: String, categoryTarget: String): List[ String ] =
  {
    var policies = List[ String ]()
    if ( categoryTarget == ConnectionCategory.Group.toString ) {
      policies = ConnectionPolicy.GroupPostSharingEnabled.toString :: policies
    }
    else if ( categoryTarget == ConnectionCategory.App.toString ) {
      policies = ConnectionPolicy.SearchDisabled.toString :: ConnectionPolicy.CacheDisabled.toString  :: ConnectionPolicy.DeleteDisabled.toString :: ConnectionPolicy.ReferralsDisabled.toString :: policies
    }
    else if ( categoryTarget == ConnectionCategory.Business.toString ) {
      policies = ConnectionPolicy.RemoteSearchDisabled.toString :: ConnectionPolicy.CacheDisabled.toString :: policies
    }
    else if ( categorySource == ConnectionCategory.Business.toString && categoryTarget == ConnectionCategory.Person.toString ) {
      policies = ConnectionPolicy.SearchDisabled.toString :: ConnectionPolicy.BusinessProfileSharingEnabled.toString :: ConnectionPolicy.RoleSharingEnabled.toString ::policies
    }
    policies
  }

//  def createConnection(alias: String,
//    category: String,
//    connectionType: String,
//    readCnxn: AgentCnxnProxy,
//    writeCnxn: AgentCnxnProxy,
//    autoApprove: String,
//    policies: List[ ConnectionPolicy.Value ]): Connection =
//  {
//    new Connection(category, connectionType, alias, readCnxn, writeCnxn, autoApprove, policies)
//    // TODO eventually validation should here take place
//  }

  def createTempConnection(alias: String,
    connectionType: String,
    readCnxn: AgentCnxnProxy,
    writeCnxn: AgentCnxnProxy): Connection =
  {
    new Connection(ConnectionCategory.None.toString, connectionType, alias, readCnxn, writeCnxn, "true", List[ String ]())
    // TODO eventually validation should here take place
  }
}
