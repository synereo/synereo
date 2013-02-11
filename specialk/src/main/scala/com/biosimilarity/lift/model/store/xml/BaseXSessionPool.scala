package com.biosimilarity.lift.model.store.xml

import com.biosimilarity.lift.model.store.SessionPool

import org.apache.commons.pool.BasePoolableObjectFactory
import org.apache.commons.pool.impl.GenericObjectPool
import org.basex.server.ClientSession
import java.util.concurrent.{ConcurrentHashMap, Semaphore}

object BaseXSessionPool extends SessionPool[ClientSession] {
  case class BaseXClientSessionFactory(
    override val host : String, override val port : Int,
    override val user : String, override val pwd : String
  ) extends PoolableClientSessionFactory( host, port, user, pwd ) {
    override def sessionFromConfig: ClientSession = {
      new ClientSession( host, port, user, pwd )
    }
  }

  override def manufactureClientSessionFactory(
    host : String, port : Int,
    user : String, pwd : String
  ) : PoolableClientSessionFactory = {
    BaseXClientSessionFactory( host, port, user, pwd )
  }
}
