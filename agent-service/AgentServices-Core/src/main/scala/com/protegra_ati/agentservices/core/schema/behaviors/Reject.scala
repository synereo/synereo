package com.protegra_ati.agentservices.core.schema.behaviors

import org.joda.time.DateTime

trait Reject
{
  //not on case class so we can treat them as read only unless you use the state methods

  private var rejected: DateTime = null

  def getRejected =
  {rejected}

  def setRejected(source: DateTime) =
  {rejected = source}

  def isRejected() =
  {rejected != null}

  def reject(): Unit =
  {
    if ( !isRejected() )
      rejected = new DateTime()
  }
}
