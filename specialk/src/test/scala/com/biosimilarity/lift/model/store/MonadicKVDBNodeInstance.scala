package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.lib.BasicLogService
import com.biosimilarity.lift.model.store.MonadicKVDBNodeSetup._

import scala.collection.mutable
import scala.util.continuations._

object MonadicKVDBNodeInstance extends CnxnString[String, String, String] with Serializable {

  val node1 = setup("localhost", 5672, "localhost", 5672) match {
    case Left(n) => n
    case _       => throw new Exception("!")
  }

  def doGet(termStr: String) =
    for (term <- fromTermString(termStr)) {
      reset {
        for (e <- node1.get(term)) { BasicLogService.tweet(e) }
      }
    }

  def doGetAlt(termStr: String): Option[List[Option[mTT.Resource]]] =
    fromTermString(termStr).map { (term: CnxnCtxtLabel[String, String, String]) =>
      reset(node1.get(term))
    }

  def returnGet(termStr: String): List[Option[mTT.Resource]] = {
    val returnValue: mutable.ListBuffer[Option[mTT.Resource]] = mutable.ListBuffer()
    for (term <- fromTermString(termStr)) {
      reset {
        for (e <- node1.get(term)) {
          returnValue += e
          ()
        }
      }
    }
    returnValue.toList
  }

  def doPut(termStr: String, value: String) =
    for (term <- fromTermString(termStr)) {
      reset { node1.put(term, value) }
    }

  def doSubscribe(termStr: String) =
    for (term <- fromTermString(termStr)) {
      reset {
        for (e <- node1.subscribe(term)) { BasicLogService.tweet(e) }
      }
    }

  def doPublish(termStr: String, value: String) =
    for (term <- fromTermString(termStr)) {
      reset { node1.publish(term, value) }
    }
}
