package com.protegra_ati.agentservices.core.schema.persistence

import scala.reflect.BeanProperty
import com.protegra_ati.agentservices.core.schema.Data
import com.mongodb.DBObject
import com.mongodb.casbah.commons.MongoDBObject

abstract class CacheableData(_id: String, _localeCode: String, _appId: String, _exchangeKey: String)
extends Data(_id, _localeCode)
{
  def this() = this("", "", "", "")

  @BeanProperty var appId: String = _appId
  @BeanProperty var exchangeKey: String = _exchangeKey

  def addIds(_id: String, _localeCode: String, _appId: String, _exchangeKey: String) = {
    id = _id
    localeCode = _localeCode
    appId = _appId
    exchangeKey = _exchangeKey
  }

  def toCacheObject(): DBObject = {
    MongoDBObject(
      "dataId" -> id,
      "appId" -> appId,
      "exchangeKey" -> exchangeKey
    )
  }

  def toCacheKey(): DBObject = {
    MongoDBObject(
      "dataId" -> id,
      "appId" -> appId
    )
  }

  def toCacheSearchKey(): DBObject = {
    // The default implementation will return an exact-match object
    // Overriding implementations should return an object that will be used for searching
    MongoDBObject(
      "dataId" -> id,
      "appId" -> appId
    )
  }

  def fromDBObject(o: DBObject): CacheableData
}