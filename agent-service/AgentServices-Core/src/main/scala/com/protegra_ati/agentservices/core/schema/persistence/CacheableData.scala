package com.protegra_ati.agentservices.core.schema.persistence

import scala.reflect.BeanProperty
import com.protegra_ati.agentservices.core.schema.Data
import com.mongodb.DBObject
import com.mongodb.casbah.commons.MongoDBObject

abstract class CacheableData(_id: String, _localeCode: String, _brokerCnxnAppId: String, _brokerCnxnExchangeKey: String)
extends Data(_id, _localeCode)
{
  def this() = this("", "", "", "")

  @BeanProperty var brokerCnxnAppId: String = _brokerCnxnAppId
  @BeanProperty var brokerCnxnExchangeKey: String = _brokerCnxnExchangeKey

  def addIds(_id: String, _localeCode: String, _brokerCnxnAppId: String, _brokerCnxnExchangeKey: String) = {
    id = _id
    localeCode = _localeCode
    brokerCnxnAppId = _brokerCnxnAppId
    brokerCnxnExchangeKey = _brokerCnxnExchangeKey
  }

  def toCacheObject(): DBObject = {
    MongoDBObject(
      "dataId" -> id,
      "brokerCnxnAppId" -> brokerCnxnAppId,
      "brokerCnxnExchangeKey" -> brokerCnxnExchangeKey
    )
  }

  def toCacheKey(): DBObject = {
    MongoDBObject(
      "dataId" -> id,
      "brokerCnxnAppId" -> brokerCnxnAppId
    )
  }

  def toCacheSearchKey(brokerCnxnAppIds: List[String]): DBObject = {
    // The default implementation will return an exact-match object
    // Overriding implementations should return an object that will be used for searching
    MongoDBObject(
      "dataId" -> id,
      "brokerCnxnAppId" -> brokerCnxnAppId
    )
  }

  def fromDBObject(o: DBObject): CacheableData
}