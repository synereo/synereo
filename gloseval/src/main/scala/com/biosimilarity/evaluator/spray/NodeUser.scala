package com.biosimilarity.evaluator.spray

object NodeUser {
  val imgFileName = "../../../../../resources/media/queenbee64.txt"
  val imgSrc = io.Source.fromFile( imgFileName ).getLines.mkString
  val email = "splicious-design-team@googlegroups.com"
  val password = "splicious"
  val jsonBlob = """{"name":"Queen Splicious","imgSrc":"${imgSrc}"}"""
}
