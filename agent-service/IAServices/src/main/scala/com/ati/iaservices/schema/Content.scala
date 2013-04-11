package com.ati.iaservices.schema

import com.protegra_ati.agentservices.core.schema.Data
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.schema.KVDBSerializable
import java.io.Serializable
import scala.reflect.BeanProperty

case class Content[ T <: ContentValue ](@BeanProperty val value: T)
  extends Data
  with Serializable
  with KVDBSerializable
{
  def this() = this(null.asInstanceOf[T])

  var contentType = formattedContentValueName

  def formattedContentValueName: String =
  {
    if (value == null)
      return ""

    value.getClass.getName.trimPackage
  }
}

object Content
{
  final val SEARCH_ALL_KEY = new Content().toSearchKey

  final val SEARCH_ALL = new Content()
  {
    override def toSearchKey(): String = Content.SEARCH_ALL_KEY
  }
}

