package com.protegra_ati.agentservices.core.schema

import java.util.UUID
import java.util.HashMap
import scala.reflect.BeanProperty

case class IntroductionProfile(@BeanProperty var alias:String) extends Data {

  //need a no-parameter constructor to create a version of the object with only authorized fields populated
  def this() = this("")

  //TODO: The Displayable names should really come from a resource lookup by language
  //this hashmap needs to be replaced by resources

  protected override def getDisplayableFieldNames:HashMap[String,String]  = {
    val map = new HashMap[String, String]()
    map.put("alias", "Alias")
    map
  }
  override def getDisplayName:String = {
    "Introduction Profile"
  }

}
