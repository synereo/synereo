package com.protegra.agentservicesstore.extensions

import org.specs._
import org.specs.runner._
import com.protegra.agentservicesstore.extensions.StringExtensions._

class StringExtensionsTest
extends JUnit4(StringExtensionsTestSpecs)

object StringExtensionsTestSpecsRunner
extends ConsoleRunner(StringExtensionsTestSpecs)

object StringExtensionsTestSpecs extends Specification {
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
