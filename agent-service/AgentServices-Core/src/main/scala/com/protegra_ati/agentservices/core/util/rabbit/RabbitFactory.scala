package com.protegra_ati.agentservices.core.util.rabbit

import com.rabbitmq.client.ConnectionFactory

object RabbitFactory {
  final val guest = new ConnectionFactory()
  guest.setUsername("guest")
  guest.setPassword("guest")
  guest.setVirtualHost("/")
  guest.setRequestedHeartbeat(0)
}