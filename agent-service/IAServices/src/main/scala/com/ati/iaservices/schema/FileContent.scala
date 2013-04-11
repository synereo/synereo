package com.ati.iaservices.schema

import java.io.Serializable
import com.protegra_ati.agentservices.store.schema.KVDBSerializable

case class FileContent(var fileName: String)
  extends ContentValue()
  with Serializable
  with KVDBSerializable
{
  def this() = this("")
}

object FileContent
{
  final val SEARCH_ALL_KEY = new Content(new FileContent).toSearchKey

  final val SEARCH_ALL = new Content(new FileContent)
  {
    override def toSearchKey(): String = FileContent.SEARCH_ALL_KEY
  }
}