package com.biosimilarity.evaluator.importer.dtos

/**
 * Transfer object for createUserRequest.
 */
case class IntroductionConfirmationRequest(
  email: String,
  password: String,
  jsonBlob: Map[String,String],
  createBTCWallet: Boolean
) extends RequestContent