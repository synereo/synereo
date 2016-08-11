package com.biosimilarity.evaluator.importer.models

import java.util.UUID
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write


/**
 * Connections two Agents in LivelyGig.
 * @param src
 * @param target
 * @param label
 */
case class ConnectionDesc(
  src: String,
  trgt: String,
  label: Option[LabelDesc]
) {

  /**
   * Serializes to JSON.
   * @return JSON String.
   */
  implicit val formats = DefaultFormats
  def toJson = write(this)  
}

object ConnectionDesc {

  implicit val formats = DefaultFormats

  /**
   * Parses an object from JSON.
   * @param json
   * @return
   */
  def fromJson(json: String) = parse(json).extract[ConnectionDesc]

}
