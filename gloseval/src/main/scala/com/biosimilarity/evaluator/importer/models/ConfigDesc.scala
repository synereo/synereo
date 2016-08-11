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
  implicit val formats = DefaultFormats
  def toJson = write(this)  
}


object ConfigDesc {

  implicit val formats = DefaultFormats

}
