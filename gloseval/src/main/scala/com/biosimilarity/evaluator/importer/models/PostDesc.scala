package com.biosimilarity.evaluator.importer.models

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write


case class PostDesc(
  src: String,
  trgts: List[String],
  label: String,
    uid: String,
  value: String
) {

  implicit val formats = DefaultFormats
  def toJson = write(this)  
}

object PostDesc {

  implicit val formats = DefaultFormats
  def fromJson(json: String) = parse(json).extract[PostDesc]

}

case class TestPostDesc(
                     src: String,
                     trgts: List[String],
                     messagePostContent: JObject,
                     uid: Option[String],
                     labels: Option[List[String]]
                   ) {

  implicit val formats = DefaultFormats
  def toJson = write(this)
}

object TestPostDesc {

  implicit val formats = DefaultFormats
  def fromJson(json: String) = parse(json).extract[PostDesc]

}

