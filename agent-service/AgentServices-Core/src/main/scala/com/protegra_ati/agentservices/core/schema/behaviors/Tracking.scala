package com.protegra_ati.agentservices.core.schema.behaviors

import org.joda.time.DateTime

trait Tracking
{
  //not on case class so we can treat them as read only unless you use the state methods

  private var sent: DateTime = null
  def getSent = { sent }
  def setSent( source: DateTime ) = { sent = source }

  private var delivered: DateTime = null
  def getDelivered = { delivered }
  def setDelivered( source: DateTime ) = { delivered = source }

  private var viewed: DateTime = null
  def getViewed = { viewed }
  def setViewed( source: DateTime ) = { viewed = source }

  def isSent() =
  {sent != null}

  def isDelivered() =
  {delivered != null}

  def isViewed() =
  {viewed != null}

  def send(): Unit =
  {
    if ( !isSent() )
      sent = new DateTime()
  }

  def deliver(): Unit =
  {
    if ( !isDelivered() )
      delivered = new DateTime()
  }

  def view(): Unit =
  {
    if ( !isViewed() )
      viewed = new DateTime()
  }

}
