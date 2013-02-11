package com.biosimilarity.lift.model.store

import org.apache.commons.pool.BasePoolableObjectFactory
import org.apache.commons.pool.impl.GenericObjectPool
import java.util.concurrent.{ConcurrentHashMap, Semaphore}

trait SessionPool[ClientSession <: {def close() : Unit}] {
  val MAX_OBJECTS_IN_POOL = 2000
  val MAX_CREATIONS = 65

  val semaphore = new Semaphore(MAX_CREATIONS)

  abstract class PoolableClientSessionFactory(
    val host : String, val port : Int, val user : String, val pwd : String
  ) extends BasePoolableObjectFactory[ClientSession] {

      def sessionFromConfig: ClientSession

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

  def manufactureClientSessionFactory(
    host : String, port : Int,
    user : String, pwd : String
  ) : PoolableClientSessionFactory

  object PoolableClientSessionFactory {
    def apply(
      host : String, port : Int,
      user : String, pwd : String
    ) : PoolableClientSessionFactory =
      manufactureClientSessionFactory( host, port, user, pwd )
    def unapply(
      pcsf : PoolableClientSessionFactory
    ) : Option[( String, Int, String, String )] = {
      Some( ( pcsf.host, pcsf.port, pcsf.user, pcsf.pwd ) )
    }
  }

  lazy final val poolMap = new ConcurrentHashMap[String, GenericObjectPool[ClientSession]]()

  // Returns an existing pool from map of pools (or creates a new if one is missing).
  // Pools are keyd on host:port
  def getPool(host: String, port: Int, user: String, pwd: String): GenericObjectPool[ClientSession] = {
    val poolKey = host + ":" + port
    val pool = poolMap.get(poolKey)
    pool match {
      case null => {
        // No pool present.  Create a new one, and try to put it in the map without
        // clobbering the old.  If we get back null, it means we were successful; otherwise
        // we were beat by another thread, return the value returned from putIfAbsent
        val newPool =
	  new GenericObjectPool[ClientSession](
	    PoolableClientSessionFactory(host, port, user, pwd),
	    MAX_OBJECTS_IN_POOL,
	    GenericObjectPool.WHEN_EXHAUSTED_BLOCK,
	    -1, -1
	  )
        val tmpPool = poolMap.putIfAbsent(poolKey, newPool)

        tmpPool match {
          case null => newPool // key was absent
          case _ => tmpPool // pool was already present
        }
      }
      case _ => pool // pool was already present
    }
  }

  def borrowClientSession(host: String, port: Int, user: String, pwd: String): ClientSession = {
    getPool(host, port, user, pwd).borrowObject()
  }

  def returnClientSession( cs : ClientSession, host: String, port: Int, user: String, pwd: String ) = {
    getPool(host, port, user, pwd).returnObject( cs )
  }

  /**
   * Remove the client session from the pool and destroy it.  Method is used
   * in case of non BaseX exceptions, such as socket errors due to disconnects, etc.
   * @param cs
   * @param host
   * @param port
   * @param user
   * @param pwd
   */
  def evictClientSession( cs : ClientSession, host: String, port: Int, user: String, pwd: String ) = {
    try {
      getPool(host, port, user, pwd).invalidateObject(cs)
    } catch { case _ => }
  }
}
