package com.protegra_ati.agentservices.core.schema

import java.io.Serializable
import java.util.HashMap
import java.util.Properties
import java.io.File
import java.io.Reader
import java.io.FileReader
import java.io.IOException
import persistence.SearchableChildData
import scala.reflect.BeanProperty
import com.protegra.agentservicesstore.extensions._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import scala.collection.JavaConversions._


/**
 * Personal profile data object
 * @param firstName first name
 * @param lastName last name
 * @param description descr.
 * @param emailAddress valid email address
 * @param country uppercase two letter ISO-3166 country code supported by a java.util.Locale class
 * @param region country specific forms of sub-national government (e.g province, state, etc.)
 * @param city city
 * @param postalCode zip
 * @param website web site
 * @param image personal avatar picture, usually very small
 */
case class Profile(
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
//  with ProfileValidator
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

  override def getDisplayableFieldNames: HashMap[ String, String ] =
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
    map.put("image", "Image")
    map
  }

  override def getDisplayName: String =
  {
    "Profile"
  }

  override def getChildId(): String =
  {
    ""
  }

  override def getChildDataSearchKeys: java.util.List[ String ] =
  {
    List(( new AppId ).toSearchKey)
  }

  override protected def ignoredFieldsForSearchAndStoreKey(): List[ String ] =
  {List("image")}
}

object Profile
{
  def fromProperty(src: Properties): Profile =
  {
    new Profile(src.getProperty(Profile.getClass.getName.trimPackage.toCamelCase + "." + "firstName"),
      src.getProperty(Profile.getClass.getName.trimPackage.toCamelCase + "." + "lastName"),
      src.getProperty(Profile.getClass.getName.trimPackage.toCamelCase + "." + "description"),
      src.getProperty(Profile.getClass.getName.trimPackage.toCamelCase + "." + "emailAddress"),
      src.getProperty(Profile.getClass.getName.trimPackage.toCamelCase + "." + "country"),
      src.getProperty(Profile.getClass.getName.trimPackage.toCamelCase + "." + "region"),
      src.getProperty(Profile.getClass.getName.trimPackage.toCamelCase + "." + "city"),
      src.getProperty(Profile.getClass.getName.trimPackage.toCamelCase + "." + "postalCode"),
      src.getProperty(Profile.getClass.getName.trimPackage.toCamelCase + "." + "website"),
      new Image() // for the moment empty image, later may files be loaded from a file system
    )
  }


  def fromProperty(src: File): Profile =
  {
    val toBeLoaded = new Properties()
    var profile: Profile = null
    val reader: Reader = new FileReader(src);
    try {
      toBeLoaded.load(reader)
      profile = fromProperty(toBeLoaded)
    } catch {
      case ex: IOException => {
        ex.printStackTrace();
      }
    } finally {
      try {reader.close();} catch {
        case ex: Exception => {}
      }
    }
    profile
  }
}
