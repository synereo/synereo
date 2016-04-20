package com.biosimilarity.evaluator.importer.models

import org.json4s._
import org.json4s.jackson.Serialization.write

// GS - there has to be a better way, but I couldn't figure it out ...
case class SystemLabelDesc(
                            value: Option[String],
                            `type`: Option[String],
                            id: Option[String],
                            functor: Option[String],
                            components: Option[List[SystemLabelDesc]]
                          ) {
  implicit val formats = DefaultFormats
  def toJson = write(this)
}

