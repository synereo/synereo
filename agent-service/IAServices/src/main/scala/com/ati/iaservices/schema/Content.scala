package com.ati.iaservices.schema

import com.protegra_ati.agentservices.core.schema.Data
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.schema.KVDBSerializable
import java.io.Serializable
import java.util.concurrent.ConcurrentHashMap
import org.joda.time.DateTime
import com.protegra_ati.agentservices.core.schema.behaviors.Tracking

trait Annotations {
  var annotations: ConcurrentHashMap[String, Data]
}

abstract class Content() extends Data
with StorableContentDataDefaults
with Tracking
with Serializable
with KVDBSerializable {

  var created: DateTime = null
  var contentType = formattedContentValueName

  def formattedContentValueName: String = {
    this.getClass.getName.trimPackage
  }
}

