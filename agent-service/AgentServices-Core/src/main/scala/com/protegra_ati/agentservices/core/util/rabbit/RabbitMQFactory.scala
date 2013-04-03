package com.protegra_ati.agentservices.core.util.rabbit

import com.rabbitmq.client._
import collection.mutable

object RabbitMQFactory
{
  final val guest = new ConnectionFactory()
  guest.setUsername("guest")
  guest.setPassword("guest")
  guest.setVirtualHost("/")
  guest.setRequestedHeartbeat(0)

  final val _conns = new mutable.HashMap[ String, Connection ]

  def getKey(host: String, port: Int) = host + port.toString

  def getConnection(factory: ConnectionFactory, host: String, port: Int): Connection =
  {
    val key = getKey(host, port)
    _conns.getOrElse(key, createIfMissing(key, factory, host, port))
  }

  def createIfMissing(key: String, factory: ConnectionFactory, host: String, port: Int): Connection =
  {
    synchronized {
      if ( !_conns.contains(key) ) {
        println("No cached connection present, creating a new connection.")
        val conn = factory.newConnection(Array {new Address(host, port)})
        conn.addShutdownListener(new ShutdownListener {
          def shutdownCompleted(cause: ShutdownSignalException) {
            println("CONNECTION Shutdown Detected, removing connection from pool...")
            removeConnection(factory, factory.getHost, factory.getPort)
          }
        })
        _conns.put(key, conn)
        conn
      }
      else {
        _conns.get(key).head
      }
    }
  }

  protected def removeConnection(factory:ConnectionFactory, host: String, port: Int): Unit = {
    synchronized {
      val key = getKey(host, port)
      val removed = _conns.remove(key)
      println("Removed connection: " + removed)
    }
  }
}