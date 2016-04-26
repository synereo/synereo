package com.biosimilarity.evaluator.importer.dtos

import org.json4s.JsonAST.JValue

/**
 * Transfer object for createUserRequest.
 */
case class InitializeSessionResponse(
  sessionURI: String
  ,defaultAlias: String
  ,jsonBlob: JValue  //Map[String, String]
) extends RequestContent