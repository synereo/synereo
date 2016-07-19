package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.test.Generators._
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class CnxnMongoSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

  import com.biosimilarity.lift.model.store.CnxnMongoSetup._
  import com.mongodb.casbah.Imports.DBObject
  import org.json4s.JValue
  import org.json4s.jackson.JsonMethods.parse

  import scala.language.implicitConversions

  object TestHelpers {

    def unify(termA: String, termB: String): Seq[(String, String)] = {
      val ccl1: CnxnCtxtLabel[String, String, String] = CCLStringConversions(termA).toCCL()
      val ccl2: CnxnCtxtLabel[String, String, String] = CCLStringConversions(termB).toCCL()
      val mdbo1: DBObject                             = MyCCLConversions(ccl1).toMongoObject()
      try {
        CnxnMongoQuerifier()
          .queryBindings(ccl2, mdbo1)(identity, identity, identity)
          .map { (x: (String, DBObject)) =>
            (x._1, parse(x._2.toString))
          }
          .map { (y: (String, JValue)) =>
            (y._1, CnxnMongoObjectifier().fromJSON(y._2))
          }
          .map { (z: (String, CnxnCtxtLabel[String, String, String])) =>
            (z._1, z._2.toString)
          }
      } catch {
        case e: Throwable => Nil
      }
    }
  }

  "A CnxnMongoQuerifier" should {

    "perform unification (1)" in {

      val termA: String = """t1(a(1), b("a string is born"), c(true))"""
      val termB: String = """t1(c(X))"""

      val result: Seq[(String, String)] = TestHelpers.unify(termA, termB)

      result should equal(List(("c", "true")))
    }

    "perform unification (2)" in {

      val termA: String = """t1(a(1), b("a string is born"), c(true))"""
      val termB: String = """t1(a(1), b(X), c(true))"""

      val result: Seq[(String, String)] = TestHelpers.unify(termA, termB)

      result should equal(List(("b", "a string is born")))
    }

    "perform unification (3)" in {

      val termA: String = """t1(a(1), b("a string is born"), c("and it is beautiful"))"""
      val termB: String = """t1(a(1), b(X), c(Y))"""

      val result = TestHelpers.unify(termA, termB)

      result should equal(List(("b", "a string is born"), ("c", "and it is beautiful")))
    }

    "perform unification (4)" in {

      val termA: String = """t1(a(1), b("a string is born"), c(true))"""
      val termB: String = """t2(c(X))"""

      val result: Seq[(String, String)] = TestHelpers.unify(termA, termB)

      result should equal(List())
    }

    "perform unification (5)" in {

      val termA: String = """t1(a(1), b("a string is born"), c("and it is beautiful"))"""
      val termB: String = """t1(a(1), d(X), e(Z))"""

      val result = TestHelpers.unify(termA, termB)

      result should equal(List())
    }

    "perform unification (6)" in {

      val termA: String = """a(o)"""
      val termB: String = """a(X)"""

      val result = TestHelpers.unify(termA, termB)

      result should equal(List(("a", "o")))
    }

    "perform unification on randomly generated values (1)" in {

      forAll(nonEmptyLowercaseString, sentenceLike) { (functor: String, groundTerm: String) =>
        whenever(functor.nonEmpty && groundTerm.nonEmpty) {
          val termA: String = s"""$functor("$groundTerm")"""
          val termB: String = s"""$functor(X)"""
          val result        = TestHelpers.unify(termA, termB)
          result should equal(List((functor, groundTerm)))
        }
      }
    }

    "perform unification on randomly generated values (2)" in {

      forAll(nonEmptyLowercaseString, nonEmptyLowercaseString, sentenceLike, nonEmptyLowercaseString, sentenceLike) {
        (outerFunctor: String, functor1: String, groundTerm1: String, functor2: String, groundTerm2: String) =>
          whenever(List(outerFunctor, functor1, groundTerm1, functor2, groundTerm2).forall(_.nonEmpty)) {
            val termA: String = s"""$outerFunctor($functor1("$groundTerm1"), $functor2("$groundTerm2"))"""
            val termB: String = s"""$outerFunctor($functor1(X), $functor2(Y))"""
            val result        = TestHelpers.unify(termA, termB)
            result should equal(List((functor1, groundTerm1), (functor2, groundTerm2)))
          }
      }
    }
  }
}
