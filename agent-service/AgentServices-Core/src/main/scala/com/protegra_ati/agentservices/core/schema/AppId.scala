package com.protegra_ati.agentservices.core.schema

import scala.reflect.BeanProperty
import java.util.{UUID}

//id will come from data
//concrete instance until we come up with a better way of DisclosedData, right now it's by class
//ideally a generic AppId but not one that's seen by other apps
case class AppId(@BeanProperty val name: String)
  extends Data
  //TODO: with ExcludeFromAudit
{
  def this() = this("")
}

object AppId
{

  def emptyImmutableAppId() =
  {
    new AppId("")
  }

}