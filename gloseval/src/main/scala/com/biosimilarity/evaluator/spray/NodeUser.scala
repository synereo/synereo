package com.biosimilarity.evaluator.spray

object NodeUser {
  val imgFileName = "./src/main/resources/media/queenbee64.txt"
  val imgSrc = io.Source.fromFile( imgFileName ).getLines.mkString
  val email = "splicious-design-team@googlegroups.com"
  val password = "splicious"
  val jsonBlob = s"""{"name":"Queen Splicious","imgSrc":"${imgSrc}"}"""
}
