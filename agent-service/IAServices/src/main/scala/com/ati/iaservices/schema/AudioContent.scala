package com.ati.iaservices.schema

import java.io.Serializable
import com.protegra_ati.agentservices.store.schema.KVDBSerializable

case class AudioContent(var audioSrc: String, var audioType: String, var title: String) extends Content
with Serializable
with KVDBSerializable {
  def this() = this("", "", "")
}

object AudioContent {
  final val SEARCH_ALL_KEY = new AudioContent().toSearchKey

  final val SEARCH_ALL = new AudioContent {
    override def toSearchKey: String = AudioContent.SEARCH_ALL_KEY
  }
}