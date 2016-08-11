package com.biosimilarity.evaluator.importer.models

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write

case class DataSetDesc(
  agents: List[AgentDesc],
  labels: Option[List[JObject]],
  cnxns: Option[List[ConnectionDesc]],
  posts: Option[List[PostDesc]]
) {
  implicit val formats = DefaultFormats
  def toJson = write(this)  
}

object DataSetDesc {
  implicit val formats = DefaultFormats
  def fromJson(json: String) = parse(json).extract[DataSetDesc]
}

