package com.biosimilarity.evaluator.importer.dtos

import org.json4s.JObject

case class CreateUserRequest(
  email: String,
  password: String,
  jsonBlob: JObject
) extends RequestContent

case class GetUserRequest(
  email: String,
  password: String
) extends RequestContent

case class UpdateUserRequest(
  sessionURI: String,
  jsonBlob: JObject
) extends RequestContent
