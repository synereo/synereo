package com.protegra_ati.agentservices.core.schema

import java.util.HashMap
import persistence.SearchableChildData
import scala.reflect.BeanProperty
import scala.collection.JavaConversions._
import java.util.{Properties}

/**
 * Personal profile data object
 * @param firstName first name
 * @param lastName  last name
 * @param emailAddress valid email address of type
 * @param country uppercase two letter ISO-3166 country code supported by a java.util.Locale class
 * @param region country specific forms of sub-national government (e.g province, state, etc.)
 * @param city city 
 * @param renamedField just renamed
 */
case class SimpleMockProfile2(
  @BeanProperty var firstName: String,
  @BeanProperty var lastName: String,
  @BeanProperty var description: String,
  @BeanProperty var emailAddress: String,
  @BeanProperty var country: String,
  @BeanProperty var region: String,
  @BeanProperty var city: String,
  @BeanProperty var renamedField: String
   )
  extends MockData
  {
  
  //need a no-parameter constructor to create a version of the object with only authorized fields populated
  def this() = this("", "", "", "", "", "", "", "")

}

object MockProfile2
{

}
