package com.protegra_ati.agentservices.core.schema


import com.protegra_ati.agentservices.core.schema.validator.ConnectionValidator
import com.protegra_ati.agentservices.core.schema._
import scala.reflect.BeanProperty
import scala.collection.JavaConversions._
import com.protegra.agentservicesstore.extensions.StringExtensions._

case class MockConn(
  @BeanProperty val category: String,
  @BeanProperty val connectionType: String,
  @BeanProperty val alias: String,
  @BeanProperty val readCnxn: MockAgentCnxnProxy,
  @BeanProperty val writeCnxn: MockAgentCnxnProxy,
  @BeanProperty val autoApprove: String,
  @BeanProperty val policies: java.util.List[ String ]) extends Data
// @BeanProperty var policies: java.util.List[ ConnectionPolicy.Value ]) extends Data
{


  def this() = this("", "", "", null, null, "", null)
}


object MockConnFactory
{

  def createConnection(alias: String, category: String, connectionType: String, sourceId: String, targetId: String): MockConn =
  {
    createConnection(alias, category, connectionType, sourceId, targetId, "false")
  }

  def createConnection(alias: String, category: String, connectionType: String, sourceId: String, targetId: String, autoApprove: String): MockConn =
  {
    //not sure if I'm happy with this spot for the logic
    var policies = List[ String ]()
    if ( category == ConnectionCategory.Group.toString ) {
      policies = ConnectionPolicy.DataSharingEnabled.toString :: policies
    }
    createConnection(alias, category, connectionType, sourceId, targetId, autoApprove, policies)
  }

  def createConnection(alias: String,
    category: String,
    connectionType: String,
    sourceId: String,
    targetId: String,
    autoApprove: String,
    policies: List[ String ]): MockConn =
  {
    val readCnxn = new MockAgentCnxnProxy(targetId.toURI, "", sourceId.toURI)
    val writeCnxn = new MockAgentCnxnProxy(sourceId.toURI, "", targetId.toURI)
    new MockConn(category, connectionType, alias, readCnxn, writeCnxn, autoApprove, policies)
    // TODO eventually validation should here take place
  }


  def createConnection(alias: String,
    category: String,
    connectionType: String,
    readCnxn: MockAgentCnxnProxy,
    writeCnxn: MockAgentCnxnProxy, autoApprove: String): MockConn =
  {
    new MockConn(category, connectionType, alias, readCnxn, writeCnxn, autoApprove, List[ String ]())
    // TODO eventually validation should here take place
  }

}