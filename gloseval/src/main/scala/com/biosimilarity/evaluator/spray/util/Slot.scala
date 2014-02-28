package com.biosimilarity.evaluator.spray.util

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn

import com.biosimilarity.lift.model.store._

trait SlotT {
  // Failure continuations receive a reason.
  // The method fetch() must not have side effects.
  def fetch(succeed: String => Unit, fail: String => Unit): Unit
  def put(value: String, succeed: Unit => Unit, fail: String => Unit): Unit
}

class DieselSlot(pac: PortableAgentCnxn, label: CnxnCtxtLabel[String,String,String]) extends SlotT {
  import com.biosimilarity.evaluator.spray.EvalHandlerService
  import com.biosimilarity.evaluator.distribution.DSLCommLink.mTT
  import com.biosimilarity.evaluator.distribution.ConcreteHL._
  
  def fetch(succeed: String => Unit, fail: String => Unit = println _): Unit = {
    EvalHandlerService.agentMgr().fetch(
      label,
      List(pac),
      (optRsrc) => {
        optRsrc match {
          case None => ()
          case Some(mTT.RBoundHM(Some(mTT.Ground( v )), _)) => {
            v match {
              case Bottom => fail("Undefined")
              case PostedExpr( (PostedExpr( s : String ), _, _, _) ) => succeed(s)
              case _ => fail("Unrecognized resource: " + optRsrc)
            }
          }
          case _ => fail("Unrecognized resource: " + optRsrc)
        }
      }
    )
  }
  def put(value: String, succeed: Unit => Unit, fail: String => Unit): Unit = {
    EvalHandlerService.agentMgr().put(
      label,
      List(pac),
      value,
      (optRsrc) => {
        optRsrc match {
          case None => ()
          case Some(mTT.RBoundHM(Some(mTT.Ground( _ )), _)) => succeed()
          case _ => fail("Unrecognized resource: " + optRsrc)
        }
      }
    )
  }
}

class LoggingForwarderSlot(target: SlotT, onPut: String => Unit) extends SlotT {
  def fetch(succeed: String => Unit, fail: String => Unit = println _): Unit = {
    target.fetch(succeed, fail)
  }
  def put(value: String, succeed: Unit => Unit, fail: String => Unit): Unit = {
    target.put(value, Unit => { onPut(value); succeed() }, fail)
  }
}