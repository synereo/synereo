package com.protegra_ati.agentservices.core.schema.persistence


import com.protegra_ati.agentservices.core.schema.Data
import com.mongodb.DBObject
import com.mongodb.casbah.commons.MongoDBObject

abstract class CacheableData extends Data
{
  def toCacheObject(appId: String): DBObject

  def toCacheKey(appId: String): DBObject = {
    MongoDBObject(
      "dataId" -> id,
      "appId" -> appId
    )
  }

  def fromDBObject(o: DBObject): CacheableData
}
