package com.biosimilarity.lift.model.store.xml

import org.apache.commons.pool.BasePoolableObjectFactory
import org.apache.commons.pool.impl.GenericObjectPool
import org.basex.server.ClientSession
import java.util.concurrent.Semaphore

class BaseXSessionPool(host: String, port: Int, user: String, pwd: String) {
  private val MAX_OBJECTS_IN_POOL = 2000
  private val MAX_CREATIONS = 65

  @transient
  private lazy val _session = sessionFromConfig

  private val semaphore = new Semaphore(MAX_CREATIONS)

  private def sessionFromConfig: ClientSession = {
    new ClientSession(host, port, user, pwd)
  }

  case class PoolableClientSessionFactory() extends BasePoolableObjectFactory[ClientSession] {
    override def makeObject() : ClientSession = {
      semaphore.acquire
      try {
        sessionFromConfig
      }
      finally {
        semaphore.release
      }
    }

    override def validateObject(obj: ClientSession) = super.validateObject(obj)

    override def activateObject(obj: ClientSession) {
      super.activateObject(obj)
    }

    override def passivateObject(obj: ClientSession) {
      super.passivateObject(obj)
    }

    override def destroyObject(obj: ClientSession) {
      if (obj != null) {
        // Try to close the object, doing nothing on failure
        try {
          obj.close
        } catch { case _ => }
      }
    }
  }

  // No max # of objects in pool, no max # of idle objects in pool
  @transient
  lazy val _pool = new GenericObjectPool[ClientSession]( PoolableClientSessionFactory(), MAX_OBJECTS_IN_POOL, GenericObjectPool.WHEN_EXHAUSTED_BLOCK, -1, -1 )

  def borrowClientSession: ClientSession = {
    try {
      _pool.borrowObject()
    }
    catch {
      case e : java.lang.IllegalStateException => {
        _session
      }
    }
  }

  def returnClientSession( cs : ClientSession ) = {
    if ( cs != _session ) {
      _pool.returnObject( cs )
    }
  }
}
