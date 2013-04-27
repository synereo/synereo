package com.ati.iaservices.schema

import com.protegra_ati.agentservices.core.schema.Data
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.schema.KVDBSerializable
import java.io.Serializable

abstract class Content() extends Data
  with Serializable
  with KVDBSerializable {
  var contentType = formattedContentValueName

  def formattedContentValueName: String = {
    this.getClass.getName.trimPackage
  }
}

