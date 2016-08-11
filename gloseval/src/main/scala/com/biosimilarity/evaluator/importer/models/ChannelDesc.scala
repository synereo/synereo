package com.biosimilarity.evaluator.importer.models

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write


/**
 * Describes a Channel in LivelyGig.
 * @param url
 * @param channelType
 */
case class ChannelDesc(
  url: String,
  channelType: String
) {

  /**
   * Serializes to JSON.
   * @return JSON String.
   */

  implicit val formats = DefaultFormats
  def toJson = write(this)  
}

object ChannelDesc {

  implicit val formats = DefaultFormats

  /**
   * Parses an object from JSON.
   * @param json
   * @return
   */
  def fromJson(json: String) = parse(json).extract[ChannelDesc]

}
