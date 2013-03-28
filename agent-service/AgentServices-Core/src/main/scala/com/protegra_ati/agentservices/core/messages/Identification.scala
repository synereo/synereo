package com.protegra_ati.agentservices.core.messages

import java.util.UUID
import com.protegra_ati.agentservices.core.util.serializer.UseKryoSerialization

case class Identification(var id: String, var parentId: String, var conversationId: String) extends UseKryoSerialization
{
  def this(id: String, parentId: String) = this (id, parentId, parentId)

  def this(id: String) = this (id, id)

  def this() = this (UUID.randomUUID.toString)

  def copyAsChild(): Identification = {
    new Identification(UUID.randomUUID().toString, id, conversationId)
  }

}


object Identification
{

  final val SEARCH_ALL = new Identification("", "", "")

  def searchForId(id: String): Identification =
  {
    return new Identification(id, "", "")
  }

  def searchForParentId(parentId: String): Identification =
  {
    return new Identification("", parentId, "")
  }

  def searchForConversationId(conversationId: String): Identification =
  {
    return new Identification("", "", conversationId)
  }

}
