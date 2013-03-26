/*
package com.biosimilarity.lift.model.store.mongo

import com.biosimilarity.lift.model.store.SessionPool
import com.biosimilarity.lift.model.store.URIFromConfigurationT

import com.mongodb.casbah.Imports._

import java.net.URI

class MongoSessionPoolC(
  val maxObjsInPool : Int,
  val maxCreations : Int,
  implicit override val configInfo : URIFromConfigurationT
) extends SessionPool[MongoClient](
  maxObjsInPool, maxCreations, configInfo
) {
  import MongoConversions._
  override val scheme : String = "mongodb"

  case class MongoClientSessionFactory(
    override val host : String, override val port : Int,
    override val user : String, override val pwd : String
  ) extends PoolableClientSessionFactory( host, port, user, pwd ) {
    override def getSession : MongoClient = {      
      MongoClient( MongoUtils.getSessionURIFromTuple( host, port, user, pwd ) )
    }
  }

  override def manufactureClientSessionFactory(
    host : String, port : Int,
    user : String, pwd : String
  ) : PoolableClientSessionFactory = {
    MongoClientSessionFactory( host, port, user, pwd )
  }  
}

object MongoSessionPool extends MongoSessionPoolC( 2000, 65, MongoConfigInfo )
*/
