package com.biosimilarity.evaluator.importer.dtos

/**
 * Transfer object for createUserRequest.
 */
case class ResetDatabaseRequest(
  sessionURI: String,
  mongodbPath: String
) extends RequestContent