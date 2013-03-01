package com.protegra_ati.agentservices.core.schema.behaviors

import org.joda.time.DateTime

trait View
{
  private var viewed: DateTime = null

  def getViewed = { viewed }
  def setViewed(source: DateTime) { viewed = source }
  def isViewed = { viewed != null }

  def view() {
    if ( !isViewed )
      viewed = new DateTime()
  }
}
