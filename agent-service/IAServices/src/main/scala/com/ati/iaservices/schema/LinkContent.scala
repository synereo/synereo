package com.ati.iaservices.schema

import java.io.Serializable
import com.protegra_ati.agentservices.store.schema.KVDBSerializable

case class LinkContent(var url: String)
  extends ContentValue()
  with Serializable
  with KVDBSerializable
{
  def this() = this("")
}

object LinkContent
{
  final val SEARCH_ALL_KEY = new Content(new LinkContent).toSearchKey

  final val SEARCH_ALL = new Content(new LinkContent)
  {
    override def toSearchKey(): String = LinkContent.SEARCH_ALL_KEY
  }
}