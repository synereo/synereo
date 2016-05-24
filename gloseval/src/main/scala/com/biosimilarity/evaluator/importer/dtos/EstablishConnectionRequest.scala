package com.biosimilarity.evaluator.importer.dtos

case class EstablishConnectionRequest(
  sessionURI: String,
  aURI: String,
  bURI: String,
  label: String
                                     ) extends RequestContent