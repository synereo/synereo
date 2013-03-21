package com.protegra_ati.agentservices.core.schema

import com.protegra_ati.agentservices.store.mongo.usage.AgentKVDBMongoScope.acT._
import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.core.schema.validator.ConnectionValidator
import com.protegra_ati.agentservices.core.schema._
import persistence.StorableConnectionDataDefaults
import scala.reflect.BeanProperty
import org.joda.time.DateTime


case class Connection(
  @BeanProperty var category: String,
  @BeanProperty var connectionType: String,
  @BeanProperty var alias: String,
  @BeanProperty var readCnxn: AgentCnxnProxy,
  @BeanProperty var writeCnxn: AgentCnxnProxy,
  @BeanProperty var autoApprove: String,
  @BeanProperty var policies: java.util.List[ String ],
  @BeanProperty var created: DateTime
  ) extends Data
with ConnectionValidator
with StorableConnectionDataDefaults
with UINotifiable
{
//  // TODO it necessary to provide EmptyConnection object according to a Null-Pattern to use them for cases where Connection is not important instead of typing "null"
//  // TODO (cont. of the first line) and also to keep the number of objects in a memory reasonable, using singleton pattern
//  // TODO (cont.) also behaviors - list has to be a Nil
//  // TODO AgentCnxnProxy will probably also will be refactored to string, or AgentCnxnProxy will prowide toString method and also AgentCnxnProxy.valueOf methods
//  def this(connectionType: String, alias: String, readCnxn: AgentCnxnProxy, writeCnxn: AgentCnxnProxy, autoApprove: String, behaviors: List[ String ]) = this(connectionType, alias, readCnxn, writeCnxn, autoApprove, behaviors, null)
//
  def this(
  _category: String,
  _connectionType: String,
  _alias: String,
  _readCnxn: AgentCnxnProxy,
  _writeCnxn: AgentCnxnProxy,
  _autoApprove: String,
  _policies: java.util.List[ String ]) = this(_category, _connectionType, _alias, _readCnxn, _writeCnxn, _autoApprove, _policies, null)

  def this() = this(null, null, null, null, null, null, null, null)

 override def toString(): String =
  {
    {
      "Connection[ id=" + id + ", locale=" + localeCode + ", category=" + category + ", connectionType=" + connectionType + ", alias=" + alias + ", readCnxn=" + readCnxn + ", writeCnxn=" + writeCnxn + ", autoApprove=" + autoApprove + ", policies=" + ( if ( policies == null ) "null" else policies ) + ", created=" + ( if ( created == null ) "null" else created )+ "]"
    }
  }
}

object Connection
{
  final val SEARCH_ALL_KEY = new Connection().toSearchKey

  final val SEARCH_ALL = new Connection()
  {
    override def toSearchKey(): String = Connection.SEARCH_ALL_KEY
  }
}