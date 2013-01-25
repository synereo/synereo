package com.protegra_ati.agentservices.core.util

import com.protegra_ati.agentservices.store.util.{MemCache, ResultsBase}
import com.protegra_ati.agentservices.core.schema.Data
import com.protegra_ati.agentservices.core.messages.Message

object Results
  extends ResultsBase
  with DataResultsBase
  with MessageResultsBase
{
}

trait DataResultsBase
{
  self : ResultsBase =>

  def save(key: String, value: Data): Unit =
  {
    MemCache.set(key, value)(client)
  }

  def saved(key: String): Data =
  {
    saved(key, 5)
  }

  def saved(key: String, maxRetries: Int): Data =
  {
    val saved = spinlockData(key, maxRetries)
    saved match {
      case null => null
      case _ => saved
    }
  }

  def spinlockData(key: String, maxRetries: Int): Data =
  {
    var result = MemCache.get[ Data ](key)(client)
    var retries = 0
    while ( result == null && retries < maxRetries ) {
      Thread.sleep(1000)
      retries = retries + 1
      result = MemCache.get[ Data ](key)(client)
    }
    result
  }
  


}


trait MessageResultsBase
{
  self : ResultsBase =>

  def saveMessage(key: String, value: Message): Unit =
  {
    MemCache.set(key, value)(client)
  }

  def savedMessage(key: String): Message =
  {
    savedMessage(key, 5)
  }

  def savedMessage(key: String, maxRetries: Int): Message =
  {
    val saved = spinlockMessage(key, maxRetries)
    saved match {
      case null => null
      case _ => saved
    }
  }

  def spinlockMessage(key: String, maxRetries: Int): Message =
  {
    var result = MemCache.get[ Message ](key)(client)
    var retries = 0
    while ( result == null && retries < maxRetries ) {
      Thread.sleep(1000)
      retries = retries + 1
      result = MemCache.get[ Message ](key)(client)
    }
    result
  }

}

trait ListResultsBase
{
  self : ResultsBase =>

  def save(key: String, value: Data): Unit =
  {
    MemCache.set(key, value)(client)
  }

  def saved(key: String): Data =
  {
    saved(key, 5)
  }

  def saved(key: String, maxRetries: Int): Data =
  {
    val saved = spinlockData(key, maxRetries)
    saved match {
      case null => null
      case _ => saved
    }
  }

  def spinlockData(key: String, maxRetries: Int): Data =
  {
    var result = MemCache.get[ Data ](key)(client)
    var retries = 0
    while ( result == null && retries < maxRetries ) {
      Thread.sleep(1000)
      retries = retries + 1
      result = MemCache.get[ Data ](key)(client)
    }
    result
  }



}

