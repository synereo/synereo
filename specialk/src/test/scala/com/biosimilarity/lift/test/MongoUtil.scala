package com.biosimilarity.lift.test

import com.mongodb.casbah.Imports.MongoClient
import scala.collection.mutable

object MongoUtil {

  def mongoIsRunning(): Boolean =
    try {
      val mongoClient: MongoClient  = MongoClient()
      val _: mutable.Buffer[String] = mongoClient.databaseNames
      mongoClient.close()
      true
    } catch {
      case e: Throwable => false
    }
}
