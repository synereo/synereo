package com.biosimilarity.evaluator.spray

import com.mongodb.casbah.Imports.MongoClient

import scala.collection.mutable

package object util {

  def resetMongo(): Unit = {
    val mongoClient: MongoClient        = MongoClient()
    val dbNames: mutable.Buffer[String] = mongoClient.databaseNames
    dbNames.foreach { (name: String) =>
      mongoClient(name).dropDatabase()
    }
  }
}
