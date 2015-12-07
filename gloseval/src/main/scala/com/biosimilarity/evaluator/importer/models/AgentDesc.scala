package com.biosimilarity.evaluator.importer.models

import java.util.UUID
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write


/**
 * Describes a LivelyGig Agent.
 * @param id
 * @param loginId
 * @param pwd
 * @param firstName
 * @param lastName
 * @param profilePic
 * @param contacts
 * @param initialPosts
 * @param bindings
 */
case class AgentDesc(
  id: String, // TODO: agent URI will take this over once it is created
  loginId: String,
  pwd: String,
  firstName: String,
  lastName: String,
  profilePic: String,
  contacts: ContactDesc,
  initialPosts: List[PostDesc],
  bindings: Option[List[String]]
) {

  /**
   * Serializes to JSON.
   * @return JSON String.
   */
  def toJson = write(this)

  implicit val formats = DefaultFormats
}

object AgentDesc {

  implicit val formats = DefaultFormats

  /**
   * Parses an object from JSON.
   * @param json
   * @return
   */
  def fromJson(json: String) = parse(json).extract[AgentDesc]

}
