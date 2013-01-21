package com.protegra_ati.agentservices.store.extensions

import org.specs2.mutable._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._

class StringExtensionsTest extends SpecificationWithJUnit {
   "toCamelCase" should {
     "capitalize correctly" in {
        val value = "JenniferViolago"
        val expected = "jenniferViolago"

        value.toCamelCase must be_==(expected)
     }
   }

   "fromCamelCase" should {
     "capitalize correctly" in {
        val value = "jenniferViolago"
        val expected = "JenniferViolago"

        value.fromCamelCase must be_==(expected)
     }
   }

  "trimPackage" should {
    "return class" in {
      val value = "java.util.Date"
      val expected = "Date"

      value.trimPackage must be_==(expected)
    }
  }
}
