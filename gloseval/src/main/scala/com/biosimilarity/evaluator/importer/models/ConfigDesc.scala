package com.biosimilarity.evaluator.importer.models

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write


/**
 * Describes a LivelyGig dataset.
 * @param labels
 */
case class ConfigDesc(
  labels: List[LabelDesc]
) {

  /**
   * Serializes to JSON.
   * @return JSON String.
   */
  def toJson = write(this)

  implicit val formats = DefaultFormats
}


object ConfigDesc {

  implicit val formats = DefaultFormats

}
