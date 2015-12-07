package com.biosimilarity.evaluator.importer.models

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write


/**
 * Container for Contacts in LivelyGig.
 * @param channels
 */
case class ContactDesc(
  channels: List[ChannelDesc]
) {

  /**
   * Serializes to JSON.
   * @return JSON String.
   */
  def toJson = write(this)

  implicit val formats = DefaultFormats
}

object ContactDesc {

  implicit val formats = DefaultFormats

  /**
   * Parses an object from JSON.
   * @param json
   * @return
   */
  def fromJson(json: String) = parse(json).extract[ContactDesc]

}
