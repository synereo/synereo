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
 */
case class ComplexMockProfile(
  @BeanProperty var firstName: String,
  @BeanProperty var lastName: String,
  @BeanProperty var description: String,
  @BeanProperty var emailAddress: String,
  @BeanProperty var country: String,
  @BeanProperty var region: String,
  @BeanProperty var city: String,
  @BeanProperty var postalCode: String,
  @BeanProperty var website: String,
  @BeanProperty var image: Image
  )
  extends Data
  with SearchableChildData
{

  def this(firstName: String
    , lastName: String
    , description: String
    , emailAddress: String
    , country: String
    , region: String
    , city: String
    , postalCode: String
    , website: String) = this(firstName, lastName, description, emailAddress, country, region, city, postalCode, website, new Image())

  //need a no-parameter constructor to create a version of the object with only authorized fields populated
  def this() = this("", "", "", "", "", "", "", "", "", new Image())


  //TODO: The Displayable names should really come from a resource lookup by language related to a country
  //TODO: will be deleted next step
  //this hashmap needs to be replaced by resources

  protected override def getDisplayableFieldNames: HashMap[ String, String ] =
  {
    val map = new HashMap[ String, String ]()
    map.put("firstName", "First Name")
    map.put("lastName", "Last Name")
    map.put("description", "Description")
    map.put("emailAddress", "email address")
    map.put("country", "country")
    map.put("region", "region")
    map.put("city", "city")
    map.put("postalCode", "postal code")
    map.put("website", "website")
    map
  }

  override def getDisplayName: String =
  {
    "MockProfile"
  }

  override def getChildId(): String =
  {
    ""
  }

  override def getChildDataSearchKeys: java.util.List[ String ] =
  {
    List(AppId.SEARCH_ALL.toSearchKey)
  }


}

object ComplexMockProfile
{

}
