package com.biosimilarity.lift.model.store

import org.scalatest.{Matchers, WordSpec}

class MonadicKVDBNodeSpec extends WordSpec with Matchers {

  import MonadicKVDBNodeSetup.mTT

  object TestHelpers {

    import scala.collection.mutable

    type UMap = mutable.LinkedHashMap[String, CnxnCtxtLabel[String, String, String]]

    def simpleGet(getKey: String): List[String] =
      MonadicKVDBNodeInstance.returnGet(getKey).collect {
        case Some(mTT.Ground(x)) => x
      }

    def getWithQuery(getKey: String): List[(String, UMap)] =
      MonadicKVDBNodeInstance.returnGet(getKey).collect {
        case Some(mTT.RBoundHM(Some(mTT.Ground(x)), Some(y))) => (x, y)
      }

    def roundTrip(key: String, value: String): List[String] = {
      MonadicKVDBNodeInstance.doPut(key, value)
      simpleGet(key)
    }

    def roundTripWithQuery(putKey: String, value: String, getKey: String): List[(String, UMap)] = {
      MonadicKVDBNodeInstance.doPut(putKey, value)
      getWithQuery(getKey)
    }

    def translateUnifications(xs: List[UMap]): List[mutable.LinkedHashMap[String, String]] = xs.map { (x: UMap) =>
      x.map { case (k, v) => k -> v.toString }
    }

    def unificationsContainBinding(results: List[UMap], binding: (String, String)): Boolean =
      translateUnifications(results).exists { (m: mutable.LinkedHashMap[String, String]) =>
        m.exists(_ == binding)
      }
  }

  "An instance of MonadicKVDBNode" should {

    "let us put a value into the database" in {

      val key: String   = """terminatorSeries(jday(characters(name(first("Sarah"), last("Connor")))))"""
      val value: String = "They lived only to face a new nightmare: the war against the machines"

      MonadicKVDBNodeInstance.doPut(key, value) should equal(())
    }

    "let us put a value into the database with a given key and get back the same value using that key (1)" in {

      val key: String   = """terminatorSeries(jday(characters(name(first("Sarah"), last("Connor")))))"""
      val value: String = "They lived only to face a new nightmare: the war against the machines"

      val returnValues: List[String] = TestHelpers.roundTrip(key, value)

      returnValues should contain(value)
    }

    "let us put a value into the database with a given key and get back the same value using that key (2)" in {

      val key: String   = """terminatorSeries(jday(characters(name(first("John"), last("Connor")))))"""
      val value: String = "Chill out"

      val returnValues: List[String] = TestHelpers.roundTrip(key, value)

      returnValues should contain(value)
    }

    """|let us put two values into the database with related keys
       |and get back one of values using the appropriate key""".stripMargin in {

      val sarahKey: String   = """terminatorSeries(jday(characters(name(first("Sarah"), last("Connor")))))"""
      val sarahValue: String = "You came here to stop me?"
      val johnKey: String    = """terminatorSeries(jday(characters(name(first("John"), last("Connor")))))"""
      val johnValue: String  = "Yeah, I did."

      MonadicKVDBNodeInstance.doPut(sarahKey, sarahValue)
      MonadicKVDBNodeInstance.doPut(johnKey, johnValue)

      val rValues: List[String] = TestHelpers.simpleGet(sarahKey)

      rValues should contain(sarahValue)
    }

    "return List(None) when no value exists for a given key" in {

      val key: String = """terminatorSeries(jday(characters(name(first("Sarah"), last("Connor")))))"""

      val returnValues: List[Option[mTT.Resource]] = MonadicKVDBNodeInstance.returnGet(key)

      returnValues should equal(List[Option[mTT.Resource]](None))
    }

    "return List(None) when no value exists for a given key (2)" in {

      val putKey: String   = """terminatorSeries(jday(characters(name(first("John"), last("Connor")))))"""
      val putValue: String = "Chill out"
      val getKey: String   = """terminatorSeries(jday(characters(name(first("Sarah"), last("Connor")))))"""

      MonadicKVDBNodeInstance.doPut(putKey, putValue)

      val returnValues: List[Option[mTT.Resource]] = MonadicKVDBNodeInstance.returnGet(getKey)

      returnValues should equal(List[Option[mTT.Resource]](None))
    }

    """|let us put a value into the database with a given key
       |and get back the same value using a Prolog query against that key""".stripMargin in {

      val unifyWithThis: String = "Connor"
      val prologVar: String     = "X"
      val putKey: String        = s"""terminatorSeries(jday(characters(name(first("John"), last("$unifyWithThis")))))"""
      val getKey: String        = s"""terminatorSeries(jday(characters(name(first("John"), last($prologVar)))))"""
      val value: String         = "No problemo"

      val (rValues, rUnifications): (List[String], List[TestHelpers.UMap]) = TestHelpers.roundTripWithQuery(putKey, value, getKey).unzip

      TestHelpers.unificationsContainBinding(rUnifications, prologVar -> unifyWithThis) should be(true)
      rValues should contain(value)
    }

    """|let us put two values into the database with related keys
       |but only get back one value using a Prolog query against their keys (a misfeature?)""".stripMargin in {

      val prologVar: String  = "X"
      val sarahKey: String   = """terminatorSeries(jday(characters(name(first("Sarah"), last("Connor")))))"""
      val sarahValue: String = "You came here to stop me?"
      val johnKey: String    = """terminatorSeries(jday(characters(name(first("John"), last("Connor")))))"""
      val johnValue: String  = "Yeah, I did."

      MonadicKVDBNodeInstance.doPut(sarahKey, sarahValue)
      MonadicKVDBNodeInstance.doPut(johnKey, johnValue)

      val getKey: String = s"""terminatorSeries(jday(characters(name(first($prologVar), last("Connor")))))"""

      val (rValues, rUnifications): (List[String], List[TestHelpers.UMap]) = TestHelpers.getWithQuery(getKey).unzip

      TestHelpers.unificationsContainBinding(rUnifications, prologVar -> "John") should be(true)
      rValues should contain(johnValue)
    }
  }
}
