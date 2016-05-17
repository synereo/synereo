package com.biosimilarity.evaluator.importer.models

import java.util.UUID
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write


case class PostDesc(
  src: String,
  trgts: List[String],
  label: String,
    uid: String,
  value: String
) {

  /**
   * Serializes to JSON.
   * @return JSON String.
   */
  implicit val formats = DefaultFormats
  def toJson = write(this)  
}

object PostDesc {

  implicit val formats = DefaultFormats

  /**
   * Parses an object from JSON.
   * @param json
   * @return
   */
  def fromJson(json: String) = parse(json).extract[PostDesc]

}

//@@GS temp hack follows

case class TestPostDesc(
                     src: String,
                     trgts: List[String],
                     text: String,
                     uid: Option[String],
                     labels: Option[List[String]]
                     //maybe  labels: List[String]
                   ) {

  /**
    * Serializes to JSON.
    * @return JSON String.
    */
  implicit val formats = DefaultFormats
  def toJson = write(this)
}

object TestPostDesc {

  implicit val formats = DefaultFormats

  /**
    * Parses an object from JSON.
    * @param json
    * @return
    */
  def fromJson(json: String) = parse(json).extract[PostDesc]

}

