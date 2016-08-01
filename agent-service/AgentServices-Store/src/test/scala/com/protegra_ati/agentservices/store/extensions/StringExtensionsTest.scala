package com.protegra_ati.agentservices.store.extensions

import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import org.scalatest.{MustMatchers, WordSpec}

class StringExtensionsTest extends WordSpec with MustMatchers {

  "toCamelCase" should {
    "capitalize correctly" in {
      val value    = "JenniferViolago"
      val expected = "jenniferViolago"
      value.toCamelCase must ===(expected)
    }
  }

  "fromCamelCase" should {
    "capitalize correctly" in {
      val value    = "jenniferViolago"
      val expected = "JenniferViolago"
      value.fromCamelCase must ===(expected)
    }
  }

  "trimPackage" should {
    "return class" in {
      val value    = "java.util.Date"
      val expected = "Date"
      value.trimPackage must ===(expected)
    }
  }
}
