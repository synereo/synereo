package com.biosimilarity.lift.lib.amqp

import collection.mutable
import com.rabbitmq.client._

object RabbitFactory
{
  final val guest = new ConnectionFactory()
  guest.setUsername("guest")
  guest.setPassword("guest")
  guest.setVirtualHost("/")
  guest.setRequestedHeartbeat(0)

  final val _conns = new mutable.HashMap[ String, Connection ]

  def getConnection(factory: ConnectionFactory, host: String, port: Int): Connection =
  {
    val key = host + port.toString
    _conns.getOrElse(key, createIfMissing(key, factory, host, port))
  }

  def createIfMissing(key: String, factory: ConnectionFactory, host: String, port: Int): Connection =
  {
    synchronized {
      if ( !_conns.contains(key) ) {
        val conn = factory.newConnection(Array {new Address(host, port)})
        _conns.put(key, conn)
        conn
      }
      else {
        _conns.get(key).head
      }
    }
  }

}