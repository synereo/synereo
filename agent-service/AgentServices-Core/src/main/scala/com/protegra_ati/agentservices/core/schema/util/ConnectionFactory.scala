package com.protegra_ati.agentservices.core.schema.util

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.protegra.agentservicesstore.AgentTS.acT._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import java.util.UUID
import java.net.URI
import com.protegra_ati.agentservices.core.schema._
import scala.collection.JavaConversions._

object ConnectionFactory
{
  def createSelfConnection(alias: String, id: String): Connection =
  {
    createConnection(alias, ConnectionCategory.Self.toString, ConnectionCategory.Self.toString, "Full", id, id)
  }

  def createConnection(alias: String, categorySource: String, categoryTarget: String, connectionType: String, sourceId: String, targetId: String): Connection =
  {
    createConnection(alias, categoryTarget, connectionType, sourceId, targetId, "true", getPolicies(categorySource,categoryTarget))
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
    val readCnxn = new AgentCnxn(targetId.toURI, "", sourceId.toURI)
    val writeCnxn = new AgentCnxn(sourceId.toURI, "", targetId.toURI)
    new Connection(category, connectionType, alias, readCnxn, writeCnxn, autoApprove, policies)
    // TODO eventually validation should here take place
  }

  def getPolicies(categorySource: String, categoryTarget: String): List[ String ] =
  {
    var policies = List[ String ]()
    if (categoryTarget == ConnectionCategory.Group.toString)
    {
      policies = ConnectionPolicy.DataSharingEnabled.toString :: policies
    }
    else if (categoryTarget == ConnectionCategory.App.toString)
    {
      policies = ConnectionPolicy.SearchDisabled.toString :: ConnectionPolicy.DeleteDisabled.toString :: ConnectionPolicy.ReferralsDisabled.toString :: policies
    }
    else if (categoryTarget == ConnectionCategory.Business.toString)
    {
      policies = ConnectionPolicy.RemoteSearchDisabled.toString :: policies
    }
    else if (categorySource == ConnectionCategory.Business.toString && categoryTarget == ConnectionCategory.Person.toString  )
    {
      policies = ConnectionPolicy.SearchDisabled.toString :: ConnectionPolicy.DataSharingEnabled.toString ::policies
    }
    policies
  }

//  def createConnection(alias: String,
//    category: String,
//    connectionType: String,
//    readCnxn: AgentCnxn,
//    writeCnxn: AgentCnxn,
//    autoApprove: String,
//    policies: List[ ConnectionPolicy.Value ]): Connection =
//  {
//    new Connection(category, connectionType, alias, readCnxn, writeCnxn, autoApprove, policies)
//    // TODO eventually validation should here take place
//  }

  def createTempConnection(alias: String,
    connectionType: String,
    readCnxn: AgentCnxn,
    writeCnxn: AgentCnxn): Connection =
  {
    new Connection(ConnectionCategory.None.toString, connectionType, alias, readCnxn, writeCnxn, "true", List[ String ]())
    // TODO eventually validation should here take place
  }

}
