package com.biosimilarity.evaluator.importer.dtos

/**
 * Transfer object for createUserRequest.
 */
case class ConfirmEmailRequest(
  token: String
) extends RequestContent