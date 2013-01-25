package com.protegra_ati.agentservices.store.util

import org.specs2.mutable._
import org.specs2.time.Duration
import org.specs2.specification.Scope
import java.util.UUID

trait ids extends Scope
{
  val id = UUID.randomUUID()
}

class MySpecTest extends SpecificationWithJUnit
{
  println("OUTER ")

  "eventually" should {
    "A matcher can match right away with eventually" in new ids
    {
      println("ID= " + id)
      1 must eventually(be_==(1))
    }
    "A matcher can match right away with eventually, even if negated" in new ids
    {
      println("ID= " + id)
      "1" must not(beNull.eventually)
    }
    "A matcher will be retried automatically until it matches" in new ids
    {
      println("ID= " + id)
      val iterator = List(1, 2, 3).iterator
      iterator.next must be_==(3).eventually
    }
    "A matcher can work with eventually and be_== but a type annotation is necessary or a be_=== matcher" in new ids
    {
      val option: Option[ Int ] = Some(3)
      option must be_==(Some(3)).eventually(10, new Duration(2000))
    }

  }
}