package com.protegra_ati.agentservices.core.schema.behaviors

import org.joda.time.DateTime

trait Archive
{
  //not on case class so we can treat them as read only unless you use the state methods

  private var archived: DateTime = null
                        
  def getArchived =
  {archived}

  def setArchived(source: DateTime) =
  {archived = source}

  def isArchived() =
  {archived != null}

  def archive(): Unit =
  {
    if ( !isArchived() )
      archived = new DateTime()
  }
}
