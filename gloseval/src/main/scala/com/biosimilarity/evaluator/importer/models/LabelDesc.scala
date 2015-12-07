package com.biosimilarity.evaluator.importer.models

import java.util.UUID
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write


/**
 * Describes a Label in LivelyGig.
 * @param id
 * @param value
 * @param `type`
 */
case class LabelDesc(
  id: String,
  value: String,
  `type`: String
) {

  /**
   * Serializes to JSON.
   * @return JSON String.
   */
  def toJson = write(this)

  implicit val formats = DefaultFormats
}

object LabelDesc {

  implicit val formats = DefaultFormats

  /**
   * Parses an object from JSON.
   * @param json
   * @return
   */
  def fromJson(json: String) = parse(json).extract[LabelDesc]

}
