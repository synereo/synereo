package com.biosimilarity.evaluator.importer.dtos

import org.json4s._
import org.json4s.jackson.Serialization._
//import spray.http.DateTime

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
  `$type`: String,
  uid: String,
  created: String,
  modified: String,
  //connections: List[Connection],
  messagePostContent: JObject) {

  implicit val formats = DefaultFormats
  def toJson = write(this)
}
