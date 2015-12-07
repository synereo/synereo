package com.biosimilarity.evaluator.importer.dtos

/**
 * Transfer object for createUserRequest.
 */
case class CreateUserRequest(
  email: String,
  password: String,
  jsonBlob: Map[String,String],
  createBTCWallet: Boolean
) extends RequestContent