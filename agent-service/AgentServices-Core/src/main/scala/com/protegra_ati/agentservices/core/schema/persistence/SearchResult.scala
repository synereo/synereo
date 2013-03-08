package com.protegra_ati.agentservices.core.schema.persistence

import java.io.Serializable
import reflect.BeanProperty
import java.util.{Collections, List, UUID}
import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.schema._
import java.util

/**
 *
 * @param id
 * @param data search result
 * @param matchPercent relevance of the the search result in percent
 * @param brokers Broker App Id -> Broker Exchange Key mappings
 */
case class SearchResult(@BeanProperty val id: String,
                        @BeanProperty val data: CacheableData,
                        @BeanProperty var matchPercent: Double,
                        @BeanProperty var brokers: java.util.List[ Broker ],
                        @BeanProperty var existingConnection: Boolean)
  extends Serializable
{
  def this(_data: CacheableData, _matchPercent: Double, _brokers: java.util.List[ Broker ], _existingConnection: Boolean) =
    this(java.lang.System.nanoTime().toString, _data, _matchPercent, _brokers, _existingConnection)

  def this() = this("", null.asInstanceOf[CacheableData], 0.0, new util.ArrayList[ Broker ], false)
}

case class Broker(@BeanProperty val brokerAppId: String, @BeanProperty val brokerExchangeKey: String, @BeanProperty var relatedData: java.util.List[ CacheableData ]) extends Serializable
{
  def this() = this("", "", new util.ArrayList[CacheableData]())
  def this(_brokerAppId: String, _brokerExchangeKey: String) = this(_brokerAppId, _brokerExchangeKey, new util.ArrayList[CacheableData])
}