package com.protegra_ati.agentservices.core.schema.persistence

import scala.reflect.BeanProperty
import com.protegra_ati.agentservices.core.schema.Data
import com.mongodb.casbah.commons.Imports._
import scala.collection.JavaConversions._
import scala.collection.mutable.Builder
import scala.util.matching.Regex


abstract class CacheableData(_id: String, _localeCode: String, _brokerAppId: String, _brokerExchangeKey: String)
extends Data(_id, _localeCode)
{
  override protected def ignoredFieldsForSearchAndStoreKey(): List[ String ] = List("brokerAppId", "brokerExchangeKey") ::: super.ignoredFieldsForSearchAndStoreKey

  def this() = this("", "", "", "")

  @BeanProperty var brokerAppId: String = _brokerAppId
  @BeanProperty var brokerExchangeKey: String = _brokerExchangeKey

  def addIds(_id: String, _localeCode: String, _brokerAppId: String, _brokerExchangeKey: String) = {
    id = _id
    localeCode = _localeCode
    brokerAppId = _brokerAppId
    brokerExchangeKey = _brokerExchangeKey
  }

  def toCacheObject(): DBObject = {
    MongoDBObject(
      "dataId" -> id,
      "brokerAppId" -> brokerAppId,
      "brokerExchangeKey" -> brokerExchangeKey
    )
  }

  def toCacheKey(): DBObject = {
    MongoDBObject(
      "dataId" -> id,
      "brokerAppId" -> brokerAppId
    )
  }

  def toCacheSearchKey(brokerAppIds: List[String]): DBObject = {
    // The default implementation will return an exact-match object
    // Overriding implementations should return an object that will be used for searching
    MongoDBObject(
      "dataId" -> id,
      "brokerAppId" -> brokerAppId
    )
  }

  def fromDBObject(o: DBObject): CacheableData

  // Return as SearchResult by parsing results of an aggregate command
  def fromAggregateDBObject[T <: CacheableData](o: DBObject): SearchResult[T] = 
  {
    new SearchResult[T](fromDBObject(o).asInstanceOf[T], 0.0, getBrokers(o), false)
  }

  // Return map of broker appIds => broker exchange keys from an aggregate search result
  private def getBrokers(o: DBObject) = {
    val bcm = o.getAsOrElse[MongoDBList]("brokers", MongoDBList())
    val brokers = bcm.collect {
      case x:DBObject if x.contains("brokerAppId") && x.contains("brokerExchangeKey") => 
        new Broker(x.as[String]("brokerAppId"), x.as[String]("brokerExchangeKey"))
    }.toList
    brokers
  }

  /**
   * Add a property to query builder if the property is not null/empty
   */
  protected def addProp(propName: String, value: String, builder: Builder[(String, Any), DBObject]) = {
    Option(value).filter(!_.isEmpty).map(builder += propName -> _)
  }

  /**
   * Add a property to query builder if the property is not null/empty.  Property value will be converted
   * to a Starts With regex (ex. ^propValue.*)
   */
  protected def addPropWithRegex(propName: String, value: String, builder: Builder[(String, Any), DBObject]) = {
    Option(value).filter(!_.isEmpty).map(builder += propName -> toStartsWithRegex(_))
  }

  /**
   * Turn string into a regex, anchored at the front, with a wildcard on the end
   * "foo".ar results in ^foo.* (i.e. regex for strings starting with foo).  
   * Note: Match is case sensitive, but string is converted to lower case!
   */
  protected def toStartsWithRegex(s:String) : Regex = ("^" + s.toLowerCase + ".*").r

  def getMongoCollectionName(): String

  def getMongoCollectionIndexes(): List[DBObject]
}