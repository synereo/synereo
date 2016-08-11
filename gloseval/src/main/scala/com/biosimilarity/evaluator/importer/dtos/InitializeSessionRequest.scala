package com.biosimilarity.evaluator.importer.dtos

/**
 * Transfer object for createUserRequest.
 */
case class InitializeSessionRequest(
  agentURI: String
) extends RequestContent