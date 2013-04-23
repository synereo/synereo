package com.ati.iaservices.schema

import java.io.Serializable
import com.protegra_ati.agentservices.store.schema.KVDBSerializable

case class MessageContent(var text: String) extends Content
with Serializable
with KVDBSerializable {
  def this() = this("")
}

object MessageContent {
  final val SEARCH_ALL_KEY = new MessageContent().toSearchKey

  final val SEARCH_ALL = new MessageContent {
    override def toSearchKey: String = MessageContent.SEARCH_ALL_KEY
  }
}