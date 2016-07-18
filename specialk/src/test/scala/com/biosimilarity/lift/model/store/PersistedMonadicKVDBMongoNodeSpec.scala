package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.test.Generators._
import com.biosimilarity.lift.test.{AMQPUtil, MongoUtil}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, WordSpec}

class PersistedMonadicKVDBMongoNodeSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

  import PersistedMonadicKVDBMongoNodeSetup.mTT

  object TestHelpers {

    import scala.collection.mutable

    type UMap = mutable.LinkedHashMap[String, CnxnCtxtLabel[String, String, String]]

    def simpleGet(getKey: String): List[String] =
      PersistedMonadicKVDBMongoNodeInstance.returnGet(getKey).collect {
        case Some(mTT.Ground(x)) => x
      }

    def getWithQuery(getKey: String): List[(String, UMap)] =
      PersistedMonadicKVDBMongoNodeInstance.returnGet(getKey).collect {
        case Some(mTT.RBoundHM(Some(mTT.Ground(x)), Some(y))) => (x, y)
      }

    def roundTrip(key: String, value: String): List[String] = {
      PersistedMonadicKVDBMongoNodeInstance.doPut(key, value)
      simpleGet(key)
    }

    def roundTripWithQuery(putKey: String, value: String, getKey: String): List[(String, UMap)] = {
      PersistedMonadicKVDBMongoNodeInstance.doPut(putKey, value)
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

  assume(MongoUtil.mongoIsRunning(), "Could not connect to MongoDB")
  assume(AMQPUtil.rabbitIsRunning(), "Could not connect to RabbitMQ")

  "An instance of PersistedMonadicKVDBMongoNode" should {

    "let us put a value into the database" in {

      PersistedMonadicKVDBMongoNodeInstance.mc1.drop()

      val key: String   = """terminatorSeries(jday(characters(name(first("Sarah"), last("Connor")))))"""
      val value: String = "They lived only to face a new nightmare: the war against the machines"

      PersistedMonadicKVDBMongoNodeInstance.doPut(key, value) should equal(())
    }

    "let us put a value into the database with a given key and get back the same value using that key (1)" in {

      PersistedMonadicKVDBMongoNodeInstance.mc1.drop()

      val key: String   = """terminatorSeries(jday(characters(name(first("Sarah"), last("Connor")))))"""
      val value: String = "They lived only to face a new nightmare: the war against the machines"

      val returnValues: List[String] = TestHelpers.roundTrip(key, value)

      returnValues should contain(value)
    }

    "let us put a value into the database with a given key and get back the same value using that key (2)" in {

      PersistedMonadicKVDBMongoNodeInstance.mc1.drop()

      val key: String   = """terminatorSeries(jday(characters(name(first("John"), last("Connor")))))"""
      val value: String = "Chill out"

      val returnValues: List[String] = TestHelpers.roundTrip(key, value)

      returnValues should contain(value)
    }

    """|let us put two values into the database with related keys
       |and get back one of values using the appropriate key""".stripMargin in {

      PersistedMonadicKVDBMongoNodeInstance.mc1.drop()

      val sarahKey: String   = """terminatorSeries(jday(characters(name(first("Sarah"), last("Connor")))))"""
      val sarahValue: String = "You came here to stop me?"
      val johnKey: String    = """terminatorSeries(jday(characters(name(first("John"), last("Connor")))))"""
      val johnValue: String  = "Yeah, I did."

      PersistedMonadicKVDBMongoNodeInstance.doPut(sarahKey, sarahValue)
      PersistedMonadicKVDBMongoNodeInstance.doPut(johnKey, johnValue)

      val rValues: List[String] = TestHelpers.simpleGet(sarahKey)

      rValues should contain(sarahValue)
    }

    "return List(None) when no value exists for a given key" in {

      PersistedMonadicKVDBMongoNodeInstance.mc1.drop()

      val key: String = """terminatorSeries(jday(characters(name(first("Sarah"), last("Connor")))))"""

      val returnValues: List[Option[mTT.Resource]] = PersistedMonadicKVDBMongoNodeInstance.returnGet(key)

      returnValues should equal(List[Option[mTT.Resource]](None))
    }

    "return List(None) when no value exists for a given key (2)" in {

      PersistedMonadicKVDBMongoNodeInstance.mc1.drop()

      val putKey: String   = """terminatorSeries(jday(characters(name(first("John"), last("Connor")))))"""
      val putValue: String = "Chill out"
      val getKey: String   = """terminatorSeries(jday(characters(name(first("Sarah"), last("Connor")))))"""

      PersistedMonadicKVDBMongoNodeInstance.doPut(putKey, putValue)

      val returnValues: List[Option[mTT.Resource]] = PersistedMonadicKVDBMongoNodeInstance.returnGet(getKey)
      returnValues should equal(List[Option[mTT.Resource]](None))
    }

    "return List(None) when no value exists for a given key (3)" in {

      PersistedMonadicKVDBMongoNodeInstance.mc1.drop()

      val putKey1: String   = """terminatorSeries(jday(characters(name(first("John"), last("Connor")))))"""
      val putValue1: String = "Chill out"
      val putKey2: String   = """terminatorSeries(jday(characters(name(first("Sarah"), last("Connor")))))"""
      val putValue2: String =
        """|On August 29th, 1997, it's gonna feel pretty fucking real to you too.
           |Anybody not wearing 2 million sunblock is gonna have a real bad day. Get it?""".stripMargin

      PersistedMonadicKVDBMongoNodeInstance.doPut(putKey1, putValue1)
      PersistedMonadicKVDBMongoNodeInstance.doPut(putKey2, putValue2)

      val getKey: String = s"""terminatorSeries(jday(characters(name(first("Kyle"), last("Reese")))))"""

      val returnValues: List[Option[mTT.Resource]] = PersistedMonadicKVDBMongoNodeInstance.returnGet(getKey)
      returnValues should equal(List[Option[mTT.Resource]](None))
    }

    """|let us put a value into the database with a given key
       |and get back the same value using a Prolog query against that key""".stripMargin in {

      PersistedMonadicKVDBMongoNodeInstance.mc1.drop()

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
       |and get back both values using a Prolog query against their keys""".stripMargin in {

      PersistedMonadicKVDBMongoNodeInstance.mc1.drop()

      val prologVar: String  = "X"
      val sarahKey: String   = """terminatorSeries(jday(characters(name(first("Sarah"), last("Connor")))))"""
      val sarahValue: String = "You came here to stop me?"
      val johnKey: String    = """terminatorSeries(jday(characters(name(first("John"), last("Connor")))))"""
      val johnValue: String  = "Yeah, I did."

      PersistedMonadicKVDBMongoNodeInstance.doPut(sarahKey, sarahValue)
      PersistedMonadicKVDBMongoNodeInstance.doPut(johnKey, johnValue)

      val getKey: String = s"""terminatorSeries(jday(characters(name(first($prologVar), last("Connor")))))"""

      val (rValues, rUnifications): (List[String], List[TestHelpers.UMap]) = TestHelpers.getWithQuery(getKey).unzip

      TestHelpers.unificationsContainBinding(rUnifications, prologVar -> "Sarah") should be(true)
      TestHelpers.unificationsContainBinding(rUnifications, prologVar -> "John") should be(true)
      rValues should contain(sarahValue)
      rValues should contain(johnValue)
    }

    """|let us put two values into the database with related keys
       |and get back nothing using a Prolog query against their keys which shouldn't have any matches""".stripMargin in {

      PersistedMonadicKVDBMongoNodeInstance.mc1.drop()

      val prologVar: String  = "X"
      val sarahKey: String   = """terminatorSeries(jday(characters(name(first("Sarah"), last("Connor")))))"""
      val sarahValue: String = "You came here to stop me?"
      val johnKey: String    = """terminatorSeries(jday(characters(name(first("John"), last("Connor")))))"""
      val johnValue: String  = "Yeah, I did."

      PersistedMonadicKVDBMongoNodeInstance.doPut(sarahKey, sarahValue)
      PersistedMonadicKVDBMongoNodeInstance.doPut(johnKey, johnValue)

      val getKey: String = s"""terminatorSeries(jday(characters(name(first($prologVar), last("Reese")))))"""

      val (rValues, rUnifications): (List[String], List[TestHelpers.UMap]) = TestHelpers.getWithQuery(getKey).unzip

      rUnifications should equal(List[TestHelpers.UMap]())
      rValues should equal(List[String]())
    }

    """|let us put a randomly-generated value with a randomly-generated key into the database
       |and get it back with an appropriate Prolog query (1)""".stripMargin in {

      forAll(nonEmptyLowercaseString, sentenceLike, sentenceLike) { (functor: String, groundTerm: String, putValue: String) =>
        whenever(functor.nonEmpty && groundTerm.nonEmpty) {

          PersistedMonadicKVDBMongoNodeInstance.mc1.drop()

          val prologVar: String = "X"
          val putKey: String    = s"""$functor("$groundTerm")"""
          val getKey: String    = s"""$functor($prologVar)"""

          PersistedMonadicKVDBMongoNodeInstance.doPut(putKey, putValue)

          val (rValues, rUnifications): (List[String], List[TestHelpers.UMap]) = TestHelpers.getWithQuery(getKey).unzip

          TestHelpers.unificationsContainBinding(rUnifications, prologVar -> groundTerm) should be(true)
          rValues should contain(putValue)
        }
      }
    }

    """|let us put a randomly-generated value with a randomly-generated key into the database
       |and get it back with an appropriate Prolog query (2)""".stripMargin in {

      forAll(nonEmptyLowercaseString, nonEmptyLowercaseString, nonEmptyLowercaseString, sentenceLike, sentenceLike, sentenceLike) {
        (outerFunctor: String, functor1: String, functor2: String, groundTerm1: String, groundTerm2: String, putValue: String) =>
          whenever(List(outerFunctor, functor1, functor2, groundTerm1, groundTerm2, putValue).forall(_.nonEmpty)) {

            PersistedMonadicKVDBMongoNodeInstance.mc1.drop()

            val prologVar: String = "X"
            val putKey: String    = s"""$outerFunctor($functor1("$groundTerm1"), $functor2("$groundTerm2"))"""
            val getKey: String    = s"""$outerFunctor($functor1($prologVar), $functor2("$groundTerm2"))"""

            PersistedMonadicKVDBMongoNodeInstance.doPut(putKey, putValue)

            val (rValues, rUnifications): (List[String], List[TestHelpers.UMap]) = TestHelpers.getWithQuery(getKey).unzip

            TestHelpers.unificationsContainBinding(rUnifications, prologVar -> groundTerm1) should be(true)
            rValues should contain(putValue)
          }
      }
    }

    """|let us put a randomly-generated value with a randomly-generated key into the database
       |and get it back with an appropriate Prolog query containing multiple variables""".stripMargin in {

      forAll(nonEmptyLowercaseString, nonEmptyLowercaseString, nonEmptyLowercaseString, sentenceLike, sentenceLike, sentenceLike) {
        (outerFunctor: String, functor1: String, functor2: String, groundTerm1: String, groundTerm2: String, putValue: String) =>
          whenever(List(outerFunctor, functor1, functor2, groundTerm1, groundTerm2, putValue).forall(_.nonEmpty)) {

            PersistedMonadicKVDBMongoNodeInstance.mc1.drop()

            val prologVar1: String = "X"
            val prologVar2: String = "Y"
            val putKey: String     = s"""$outerFunctor($functor1("$groundTerm1"), $functor2("$groundTerm2"))"""
            val getKey: String     = s"""$outerFunctor($functor1($prologVar1), $functor2($prologVar2))"""

            PersistedMonadicKVDBMongoNodeInstance.doPut(putKey, putValue)

            val (rValues, rUnifications): (List[String], List[TestHelpers.UMap]) = TestHelpers.getWithQuery(getKey).unzip

            TestHelpers.unificationsContainBinding(rUnifications, prologVar1 -> groundTerm1) should be(true)
            TestHelpers.unificationsContainBinding(rUnifications, prologVar2 -> groundTerm2) should be(true)
            rValues should contain(putValue)
          }
      }
    }

    """|let us put multiple randomly-generated values with randomly-generated, related keys into the database
       |and get them back with an appropriate Prolog query""".stripMargin in {

      forAll(nonEmptyLowercaseString, mapOfSentenceLikeToSentenceLike) { (functor: String, groundTermToPutValue: Map[String, String]) =>
        whenever(functor.nonEmpty) {

          PersistedMonadicKVDBMongoNodeInstance.mc1.drop()

          groundTermToPutValue.foreach {
            case (groundTerm, putValue) =>
              val putKey: String = s"""$functor("$groundTerm")"""
              PersistedMonadicKVDBMongoNodeInstance.doPut(putKey, putValue)
          }

          val prologVar: String = "X"
          val getKey: String    = s"""$functor($prologVar)"""

          val (rValues, rUnifications): (List[String], List[TestHelpers.UMap]) = TestHelpers.getWithQuery(getKey).unzip

          groundTermToPutValue.foreach {
            case (groundTerm, putValue) =>
              TestHelpers.unificationsContainBinding(rUnifications, prologVar -> groundTerm) should be(true)
              rValues should contain(putValue)
          }
        }
      }
    }
  }
}
