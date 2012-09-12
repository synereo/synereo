package com.protegra_ati.agentservices.core.schema.persistence

import org.junit._
import Assert._
import org.specs._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import java.util.UUID
import java.util.Date
import java.util.Locale
import com.protegra_ati.agentservices.core.schema._
import behaviors.Tracking
import com.protegra_ati.agentservices.core.schema.Constants._
import scala.collection.JavaConversions._
import validator.DataValidator

trait MockConnectionValidator extends DataValidator
{
  self: MockConnection =>

  override def selfValidate: List[ String ] =
  {
    println("MockConnectionValidator executed")
    var result: List[ String ] = Nil;
    return result
  }
}

class MockConnection extends Data
with MockConnectionValidator
{

  var name = "Jennifer"
  var isBlue = true
  var age = 100
  var birthDate = new Date(1, 1, 1)
  var amt = 1.2345

  //our java API needs to use java lists not scala lists
  var test0: java.util.List[String] = "a" :: "b" :: Nil
  var test0Empty: java.util.List[String] = Nil

  var test1: String = null
  var test2: Int = 0
  var test3: Boolean = false
  var test4: Date = null
  var test5: Short = 0
  var test6: Long = 0
  var test7: Float = 0
  var test8: Double = 0
  var test9 = null
  var test10 = new java.util.HashMap[String, Object]()

  def test =
  {}
}

class MockConnectionTracked extends MockConnection with Tracking
