package com.ati.iaservices.schema

import java.io.Serializable
import com.protegra_ati.agentservices.store.schema.KVDBSerializable

case class PostContent(var text: String)
  extends ContentValue()
  with Serializable
  with KVDBSerializable
{
  def this() = this("")
}

object PostContent
{
  final val SEARCH_ALL_KEY = new Content(new PostContent).toSearchKey

  final val SEARCH_ALL = new Content(new PostContent)
  {
    override def toSearchKey(): String = PostContent.SEARCH_ALL_KEY
  }
}