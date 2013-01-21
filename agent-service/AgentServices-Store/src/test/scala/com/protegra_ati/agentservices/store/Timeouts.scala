package com.protegra_ati.agentservices.store

import org.specs2.time.Duration


trait Timeouts {
  val TIMEOUT_SHORT = 200
  val TIMEOUT_MED = 500
  val TIMEOUT_LONG = 1000
  val TIMEOUT_VERY_LONG = 5000
  val TIMEOUT_BEFORE_RESULT_FETCH = 1000

  @transient val TIMEOUT_EVENTUALLY = new Duration(2000)
  @transient val TIMEOUT_EVENTUALLY_LONG = new Duration(10000)

  def trySleep(count: Int) = {
    if (count == 0)
      Thread.sleep(TIMEOUT_LONG)
  }

  def trySleep(value: Any) = {
    if (value == null || value == "")
      Thread.sleep(TIMEOUT_LONG)
  }

  def SleepToPreventContinuation() = {
    Thread.sleep(TIMEOUT_BEFORE_RESULT_FETCH)
    Thread.sleep(TIMEOUT_BEFORE_RESULT_FETCH)
    Thread.sleep(TIMEOUT_BEFORE_RESULT_FETCH)
    Thread.sleep(TIMEOUT_BEFORE_RESULT_FETCH)
    Thread.sleep(TIMEOUT_BEFORE_RESULT_FETCH)
  }
}