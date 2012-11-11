package com.protegra_ati.agentservices.core.util

import com.protegra.agentservicesstore.util.{MemCache, ResultsBase}
import com.protegra_ati.agentservices.core.schema.Data
import com.protegra_ati.agentservices.core.messages.Message

object Results extends DataResultsBase
{
}

trait DataResultsBase extends ResultsBase
{

  def save(key: String, value: Data): Unit =
  {
    MemCache.add(key, value)(client)
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
  
  
  def saveMessage(key: String, value: Message): Unit =
  {
    MemCache.add(key, value)(client)
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

