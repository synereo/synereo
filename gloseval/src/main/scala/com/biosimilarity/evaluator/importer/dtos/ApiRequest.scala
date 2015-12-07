package com.biosimilarity.evaluator.importer.dtos

/**
 * Wraps all API requests in a standard format.
 */
case class ApiRequest(
  msgType: String,
  content: RequestContent
)