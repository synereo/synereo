package com.biosimilarity.lift.test

import com.biosimilarity.lift.model.store.{CnxnCtxtBranch, CnxnCtxtLabel, CnxnCtxtLeaf, Factual}
import com.biosimilarity.lift.model.store.CnxnMongoSetup._
import com.mongodb.casbah.Imports.{DBObject, JSFunction}
import org.json4s.JValue
import org.json4s.jackson.JsonMethods.parse

import scala.language.implicitConversions

/**
  * lifted from CnxnMongo.scala
  */
object CnxnMongoExampleData extends Serializable {

  type CCLF = CnxnCtxtLabel[String, String, String] with Factual

  val ccl1: CnxnCtxtLabel[String, String, String] = CCLStringConversions("""t1(a(1), b("a string is born"), c(true))""").toCCL()
  val ccl2: CnxnCtxtLabel[String, String, String] = CCLStringConversions("""t1(a(1), b(X),                  c(true))""").toCCL()
  val mdbo1: DBObject                             = MyCCLConversions(ccl1).toMongoObject()
  val mdbo2: DBObject                             = MyCCLConversions(ccl2).toMongoObject()
  val json1: JSFunction                           = mdbo1.toString
  val json2: JSFunction                           = mdbo2.toString
  val jv1: JValue                                 = parse(json1)
  val jv2: JValue                                 = parse(json2)
  val ccl1rt: CCLF                                = CnxnMongoObjectifier().fromJSON(jv1)(identity, identity, identity)
  val ccl2rt: CCLF                                = CnxnMongoObjectifier().fromJSON(jv2)(identity, identity, identity)
  val mqry1: DBObject                             = MyCCLConversions(ccl1).toMongoQuery()
  val mqry2: DBObject                             = MyCCLConversions(ccl2).toMongoQuery()

  val varPaths = CnxnMongoQuerifier().filteredPaths(ccl2, {
    case CnxnCtxtLeaf(Left(tag))  => false
    case CnxnCtxtLeaf(Right(tag)) => true
    case CnxnCtxtBranch(_, _)     => true
  })(identity, identity, identity)

  val groundPaths = CnxnMongoQuerifier().filteredPaths(ccl2, {
    case CnxnCtxtLeaf(Left(tag))  => true
    case CnxnCtxtLeaf(Right(tag)) => false
    case CnxnCtxtBranch(_, _)     => true
  })(identity, identity, identity)

  val qbs1 = CnxnMongoQuerifier().queryBindings(ccl2, mdbo1)(identity, identity, identity)
}
