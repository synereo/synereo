package com.ati.iaservices.schema

import java.io.Serializable
import com.protegra_ati.agentservices.store.schema.KVDBSerializable

case class EmptyContent() extends Content
with Serializable
with KVDBSerializable {

}

object EmptyContent {
  final val SEARCH_ALL_KEY = new EmptyContent().toSearchKey

  final val SEARCH_ALL = new EmptyContent {
    override def toSearchKey: String = EmptyContent.SEARCH_ALL_KEY
  }
}