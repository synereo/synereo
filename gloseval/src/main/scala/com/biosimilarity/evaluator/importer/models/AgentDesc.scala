package com.biosimilarity.evaluator.importer.models

import java.util.UUID
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write

case class AgentDesc(
  id: String,
  email: String,
  pwd: String,
  jsonBlob: String,
  aliasLabels : Option[List[JObject]]
  //contacts: ContactDesc,
  //initialPosts: List[PostDesc],
  //bindings: Option[List[String]]
) {
  implicit val formats = DefaultFormats
  def toJson = write(this)  
}

object AgentDesc {
  implicit val formats = DefaultFormats
  def fromJson(json: String) = parse(json).extract[AgentDesc]
}
