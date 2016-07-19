package com.biosimilarity.lift.model.store

import org.scalatest.{Matchers, WordSpec}

class ExternalConditionsSpec extends WordSpec with Matchers {

  trait Looky[N] {
    def b: Boolean
    def s: String
    def n: N
    def r: Option[Looky[N]]
  }

  class LookyLoo(override val b: Boolean, override val s: String, override val n: Int, override val r: Option[Looky[Int]])
      extends Looky[Int]

  object LookyLoo {

    def apply(b: Boolean, s: String, n: Int, r: Option[Looky[Int]]): LookyLoo =
      new LookyLoo(b, s, n, r)

    def unapply(ll: LookyLoo): Option[(Boolean, String, Int, Option[Looky[Int]])] =
      Some((ll.b, ll.s, ll.n, ll.r))
  }

  class LookyLooToo(override val b: Boolean, override val s: String, override val n: Double, override val r: Option[Looky[Double]])
      extends Looky[Double]

  object LookyLooToo {

    def apply(b: Boolean, s: String, n: Double, r: Option[Looky[Double]]): LookyLooToo =
      new LookyLooToo(b, s, n, r)

    def unapply(ll: LookyLooToo): Option[(Boolean, String, Double, Option[Looky[Double]])] =
      Some((ll.b, ll.s, ll.n, ll.r))
  }

  "ExternalConditions" should {

    "allow us to register content as a certain type (formerly known as addContent)" in {
      val ctag1 = ExternalConditions.registerContentAsT[LookyLoo](new LookyLoo(true, "true", 1, None))
      val ctag2 = ExternalConditions.registerContentAsT[LookyLooToo](new LookyLooToo(true, "true", 1.0, None))
      (ctag1, ctag2) shouldBe a[(String, String)]
    }

    """|allow us to register content as a certain type and retrieve that content
       |as a specified type (formerly known as positiveTests)""".stripMargin in {
      val ctag1 = ExternalConditions.registerContentAsT[LookyLoo](new LookyLoo(true, "true", 1, None))
      val ctag2 = ExternalConditions.registerContentAsT[LookyLooToo](new LookyLooToo(true, "true", 1.0, None))
      val ll1   = ExternalConditions.retrieveContent[LookyLoo](ctag1).orNull
      val ll2   = ExternalConditions.retrieveContent[LookyLooToo](ctag2).orNull
      val ll3   = ExternalConditions.retrieveContent[Looky[Int]](ctag1).orNull
      val ll4   = ExternalConditions.retrieveContent[Looky[Double]](ctag2).orNull
      (ll1, ll2, ll3, ll4) shouldBe a[(LookyLoo, LookyLooToo, Looky[Int], Looky[Double])]
    }

    """|allow us to register content as certain type but be unable to receive that content
       |if we don't provide the right type variable (formerly known as negativeTests""".stripMargin in {
      val ctag1         = ExternalConditions.registerContentAsT[LookyLoo](new LookyLoo(true, "true", 1, None))
      val ctag2         = ExternalConditions.registerContentAsT[LookyLooToo](new LookyLooToo(true, "true", 1.0, None))
      val ll1: LookyLoo = ExternalConditions.retrieveContent(ctag1).orNull
      val ll2           = ExternalConditions.retrieveContent(ctag2).orNull
      val ll3           = ExternalConditions.retrieveContent[Looky[Double]](ctag1).orNull
      val ll4           = ExternalConditions.retrieveContent[Looky[Int]](ctag2).orNull
      (ll1, ll2, ll3, ll4) should equal((null, null, null, null))
    }
  }
}
