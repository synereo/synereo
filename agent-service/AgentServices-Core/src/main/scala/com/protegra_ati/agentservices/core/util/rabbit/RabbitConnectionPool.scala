package com.protegra_ati.agentservices.core.util.rabbit

import collection.mutable
import org.apache.commons.pool.BasePoolableObjectFactory
import org.apache.commons.pool.impl.GenericObjectPool
import com.rabbitmq.client._
import com.biosimilarity.lift.lib.amqp.RabbitFactory

object RabbitConnectionSingletonFactory
{

}

object RabbitConnectionPool
{
  //  @transient
  final val _pools = new mutable.HashMap[ String, GenericObjectPool[ Connection ] ]
  final val MAX_ACTIVE = 50
  final val MAX_WAIT = 60000
  final val MAX_IDLE = 60000

  final val conn = RabbitFactory.getConnection(RabbitFactory.guest, "localhost", 5672)


  case class LocalRabbitFactory(host: String, port: Int) extends BasePoolableObjectFactory[ Connection ]
  {
    override def makeObject(): Connection =
    {
      RabbitFactory.getConnection(RabbitFactory.guest, host, port)
    }
  }

  private def getPool(host: String, port: Int): GenericObjectPool[ Connection ] =
  {
    val key = host + port.toString
    //come up with a better way than sync? need to make sure only 1 miss creates a new pool
    val existing = _pools.get(key)
    existing match {
      case Some(pool) => pool
      case None => {
        //double check this time blocking - should only happen the first cache miss
        synchronized {
          val newPool = new GenericObjectPool[ Connection ](LocalRabbitFactory(host, port), MAX_ACTIVE, GenericObjectPool.WHEN_EXHAUSTED_BLOCK, MAX_WAIT, MAX_IDLE)
          putIfAbsent(key, newPool)
        }
      }
    }
  }

  private def putIfAbsent(key: String, value: GenericObjectPool[ Connection ]): GenericObjectPool[ Connection ] =
  {
    if ( !_pools.contains(key) ) {
      _pools.put(key, value)
      value
    }
    else {
      _pools.get(key).head
    }
  }

  def getConnection(host: String, port: Int): Connection =
  {
    try {
      val pool = getPool(host, port)
      val connection = pool.borrowObject()
//      println("getting conn pool active size is : " + pool.getNumActive + " idle : " + pool.getNumIdle)
      connection
    }
    catch {
      case e: java.lang.IllegalStateException => {
//        println("Exception: " + e)
        throw e
      }
    }
  }

  def dropConnection(connection: Connection) =
  {
    try {
      val pool = getPool(connection.getAddress.getHostName, connection.getPort)
//      println("returning conn pool active size is : " + pool.getNumActive + " idle : " + pool.getNumIdle)
      pool.returnObject(connection)
    }
    catch {
      case e: java.lang.IllegalStateException => {
//        println("Exception: " + e)
        throw e
      }
    }
  }
}
