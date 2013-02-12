package com.biosimilarity.lift.model.store.mongo

import com.biosimilarity.lift.model.store.SessionPool

import com.mongodb.casbah.Imports._

object MongoSessionPool extends SessionPool[MongoClient] {
  case class MongoClientSessionFactory(
    override val host : String, override val port : Int,
    override val user : String, override val pwd : String
  ) extends PoolableClientSessionFactory( host, port, user, pwd ) {
    override def sessionFromConfig: MongoClient = {
      //new ClientSession( host, port, user, pwd )
      // BUGBUG -- lgm : add in config parameters!!!
      MongoClient()
    }
  }

  override def manufactureClientSessionFactory(
    host : String, port : Int,
    user : String, pwd : String
  ) : PoolableClientSessionFactory = {
    MongoClientSessionFactory( host, port, user, pwd )
  }
}
