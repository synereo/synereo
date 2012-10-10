package com.protegra_ati.agentservices.core.schema.behaviors

import org.joda.time.DateTime

trait Ignore
{
  //not on case class so we can treat them as read only unless you use the state methods

  private var ignored: DateTime = null

  def getIgnored =
  {ignored}

  def setIgnored(source: DateTime) =
  {ignored = source}

  def isIgnored() =
  {ignored != null}

  def ignore(): Unit =
  {
    if ( !isIgnored() )
      ignored = new DateTime()
  }
}
