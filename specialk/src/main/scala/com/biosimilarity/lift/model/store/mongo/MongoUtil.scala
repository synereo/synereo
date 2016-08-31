package com.biosimilarity.lift.model.store.mongo

import com.mongodb.casbah.Imports._

import scala.collection.mutable
import scala.util.Try

object MongoUtil {

  def mongoIsRunning(host: String = "localhost", port: Int = 27017): Boolean =
    Try {
      val mongoClient: MongoClient  = MongoClient(host, port)
      val _: mutable.Buffer[String] = mongoClient.databaseNames
      mongoClient.close()
    }.isSuccess
}
