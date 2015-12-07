package com.biosimilarity.evaluator.importer.models

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write


/**
 * Describes a LivelyGig dataset.
 * @param agents
 * @param labels
 * @param cnxns
 */
case class DataSetDesc(
  agents: List[AgentDesc],
  labels: List[LabelDesc],
  cnxns: List[ConnectionDesc]
) {

  /**
   * Serializes to JSON.
   * @return JSON String.
   */
  def toJson = write(this)

  implicit val formats = DefaultFormats
}

object DataSetDesc {

  implicit val formats = DefaultFormats

  /**
   * Parses an object from JSON.
   * @param json
   * @return
   */
  def fromJson(json: String) = parse(json).extract[DataSetDesc]

}