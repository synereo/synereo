package com.biosimilarity.evaluator.importer.dtos

/**
 * Transfer object for createUserRequest.
 */
case class EstablishConnectionRequest(
  sessionURI: String,
  aURI: String,
  bURI: String
) extends RequestContent