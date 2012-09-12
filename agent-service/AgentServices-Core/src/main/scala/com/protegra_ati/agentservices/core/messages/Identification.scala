package com.protegra_ati.agentservices.core.messages

import java.util.UUID

case class Identification(val id: String, var parentId: String, var conversationId: String)
{
  def this(id: String, parentId: String) = this (id, parentId, parentId)

  def this(id: String) = this (id, id)

  def this() = this (UUID.randomUUID.toString)

  def copyAsChild(): Identification = {
    new Identification(UUID.randomUUID().toString, id, conversationId)
  }

}