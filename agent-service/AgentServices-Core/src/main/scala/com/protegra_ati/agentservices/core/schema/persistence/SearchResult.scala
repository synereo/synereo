package com.protegra_ati.agentservices.core.schema.persistence

import java.io.Serializable
import reflect.BeanProperty
import java.util.{Collections, List, UUID}
import scala.collection.JavaConversions._
import com.protegra_ati.agentservices.core.schema._

/**
 *
 * @param id
 * @param data search result
 * @param matchPercent relevance of the the search result in percent
 * @param relatedData eventually additional data some how related to the search result.
 * The relationship to the result is defined over trait 'SearchableChildData'.
 * @param brokerCnxns Broker App Id -> Broker Exchange Key mappings
 */
case class SearchResult[T <: CacheableData](@BeanProperty val id: String,
                        @BeanProperty val data: T,
                        @BeanProperty var matchPercent: Double,
                        @BeanProperty var relatedData: java.util.List[ T ],
                        @BeanProperty val brokerCnxns: java.util.List[ BrokerCnxn ],
                        @BeanProperty var existingConnection: Boolean)
  extends Serializable
{
  def this(_data: T, _matchPercent: Double, _relatedData: List[ T ], _brokerCnxns: java.util.List[ BrokerCnxn ], _existingConnection: Boolean) =
    this(java.lang.System.nanoTime().toString, _data, _matchPercent, _relatedData, _brokerCnxns, _existingConnection)

  def this(_data: T, _matchPercent: Double, _brokerCnxns: java.util.List[ BrokerCnxn ], _existingConnection: Boolean) =
    this(java.lang.System.nanoTime().toString, _data, _matchPercent, Collections.emptyList[ T ], _brokerCnxns, _existingConnection)


  def this() = this("", null.asInstanceOf[T], 0.0, Collections.emptyList[ T ], Collections.emptyList[ BrokerCnxn ], false)
}

case class BrokerCnxn(@BeanProperty val brokerCnxnAppId: String, @BeanProperty val brokerCnxnExchangeKey: String) extends Serializable
{
  def this() = this("", "")
}