package com.biosimilarity.evaluator.importer.dtos

case class CreateUserRequest(
  email: String,
  password: String,
  jsonBlob: String
) extends RequestContent

case class GetUserRequest(
  email: String,
  password: String
) extends RequestContent

case class UpdateUserRequest(
  sessionURI: String,
  jsonBlob: String
) extends RequestContent
