package com.biosimilarity.evaluator.importer.dtos

/**
 * Transfer object for createUserRequest.
 */
case class SessionPingRequest(
  sessionURI: String
) extends RequestContent