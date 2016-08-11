package com.biosimilarity.evaluator.importer.dtos

/**
 * Transfer object for createUserRequest.
 */
case class BeginIntroductionRequest(
  sessionURI: String,
  alias: String,
  aConnection: Connection,
  bConnection: Connection,
  aMessage: String,
  bMessage: String
) extends RequestContent