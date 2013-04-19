package com.ati.iaservices.schema

import java.io.Serializable
import com.protegra_ati.agentservices.store.schema.KVDBSerializable

case class ImageContent(var imgSrc: String, var caption: String) extends Content
with Serializable
with KVDBSerializable {
  def this() = this("", "")
}

object ImageContent {
  final val SEARCH_ALL_KEY = new ImageContent().toSearchKey

  final val SEARCH_ALL = new ImageContent {
    override def toSearchKey: String = ImageContent.SEARCH_ALL_KEY
  }
}