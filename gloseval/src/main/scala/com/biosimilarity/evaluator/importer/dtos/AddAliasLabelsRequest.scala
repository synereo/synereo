package com.biosimilarity.evaluator.importer.dtos

/**
 * Transfer object for createUserRequest.
 */
case class AddAliasLabelsRequest(
  sessionURI: String,
  alias: String,
  labels: List[String]
) extends RequestContent