package com.biosimilarity.evaluator.spray

import java.io.InputStream

import com.biosimilarity.evaluator.distribution.EvalConfConfig
import com.mongodb.casbah.Imports.MongoClient
import com.mongodb.casbah.MongoClientURI

import scala.collection.mutable

package object util {

  def resourceStream(resourceName: String): InputStream = {
    val is: InputStream = getClass.getClassLoader.getResourceAsStream(resourceName)
    require(is.ne(null), s"Resource $resourceName not found")
    is
  }

  def resetMongo(): Unit = {
    val dbHost = EvalConfConfig.readStringOrElse("dbHost", "localhost")
    val dbPort = EvalConfConfig.readStringOrElse("dbPort", "27017")
    val uri                             = MongoClientURI(s"mongodb://$dbHost:$dbPort/")
    val mongoClient: MongoClient        = MongoClient(uri)
    val dbNames: mutable.Buffer[String] = mongoClient.databaseNames
    dbNames.foreach { (name: String) =>
      mongoClient(name).dropDatabase()
    }
  }
}
