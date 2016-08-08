package com.biosimilarity.evaluator.spray

import java.io.InputStream

import com.mongodb.casbah.Imports.MongoClient

import scala.collection.mutable

package object util {

  def resourceStream(resourceName: String): InputStream = {
    val is: InputStream = getClass.getClassLoader.getResourceAsStream(resourceName)
    require(is.ne(null), s"Resource $resourceName not found")
    is
  }

  def resetMongo(): Unit = {
    val mongoClient: MongoClient        = MongoClient()
    val dbNames: mutable.Buffer[String] = mongoClient.databaseNames
    dbNames.foreach { (name: String) =>
      mongoClient(name).dropDatabase()
    }
  }
}
