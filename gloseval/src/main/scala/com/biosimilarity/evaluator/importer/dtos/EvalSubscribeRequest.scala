package com.biosimilarity.evaluator.importer.dtos

import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization._
import spray.http.DateTime

case class EvalSubscribeRequest(
  sessionURI: String,
  expression: EvalSubscribeExpression) extends RequestContent

case class EvalSubscribeExpression(
  msgType: String,
  content: EvalSubscribeContent)

case class EvalSubscribeContent(
  cnxns: List[Connection],
  label: String,
  value: String,
  uid: String)

case class PostContent(
  uid: String,
  `type`: String,
  created: String,
  modified: String,
  labels: List[String],
  connections: List[Connection],
  text: String) {

  /**
   * Serializes to JSON.
   *
   * @return JSON String.
   */
  implicit val formats = DefaultFormats
  def toJson = write(this)
}
