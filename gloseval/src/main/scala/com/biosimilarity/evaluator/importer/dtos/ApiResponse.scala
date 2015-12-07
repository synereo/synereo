package com.biosimilarity.evaluator.importer.dtos

/**
 * Created by justin on 2015-12-04.
 */
case class ApiResponse[T](
  msgType: String,
  content: T
)