package com.biosimilarity.evaluator.importer.dtos

import scala.collection.Map

/**
 * Transfer object for createUserRequest.
 */
case class CreateUserRequest(
  email: String,
  password: String,
  jsonBlob: scala.collection.immutable.Map[String,String],
  createBTCWallet: Boolean
) extends RequestContent

case class UpdateUserRequest(
  sessionURI: String,
  jsonBlob: scala.collection.immutable.Map[String,String]
) extends RequestContent
