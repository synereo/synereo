package com.biosimilarity.evaluator.spray

import java.net.URI
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}

class URISerializer extends Serializer[URI] {
  private val URIClass = classOf[URI]

  override def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), URI] = {
    case (TypeInfo(URIClass, _), json) => json match {
      case JString(s) => new URI(s)
      case x => throw new MappingException("Can't convert " + x + " to java.net.URI")
    }
  }

  override def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case x: URI => JString(x.toString)
  }
}

object Serializer {
  implicit val formats = Serialization.formats(NoTypeHints) + new URISerializer

  def serialize(o: AnyRef): String = {
    write(o)
  }

  def deserialize[T: Manifest](s: String): T = {
    read[T](s)
  }
}
