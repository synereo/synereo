package com.biosimilarity.evaluator.importer.dtos

/**
 * Transfer object for createUserRequest.
 */
case class InitializeSessionResponse(
  sessionURI: String,
  defaultAlias: String,
  jsonBlob: Map[String, String]
) extends RequestContent