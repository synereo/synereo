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
import java.lang.{Integer}

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
    Profile.DISPLAYABLE_FIELD_NAMES
  }

  override def getDisplayableFieldSortOrder: HashMap[ String, Integer ] =
  {
    Profile.DISPLAYABLE_FIELD_SORT_ORDER
  }


  override def getDisplayName: String =
  {
    "Profile"
  }

  override def getChildId(): String =
  {
    ""
  }

  override def getChildDataSearchKeys: java.util.List[ Data ] =
  {
    List(AppId.SEARCH_ALL)
  }

  override protected def ignoredFieldsForSearchAndStoreKey(): List[ String ] =
  {List("image")}
}

object Profile
{
  final val DISPLAYABLE_FIELD_NAMES = createDisplayableFieldNamesMap()

  final val DISPLAYABLE_FIELD_SORT_ORDER = createDisplayableFieldSortOrderMap()


  final val SEARCH_ALL_KEY = new Profile().toSearchKey

  final val SEARCH_ALL = new Profile()
  {
    override def toSearchKey(): String = Profile.SEARCH_ALL_KEY
  }


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
      Image.fromProperty(src, Profile.getClass.getName.trimPackage.toCamelCase + "." + "image")

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


  private def createDisplayableFieldNamesMap(): HashMap[ String, String ] =
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

  private def createDisplayableFieldSortOrderMap(): HashMap[ String, Integer ] =
  {
    val map = new HashMap[ String, Integer ]()
    map.put("firstName", 1)
    map.put("lastName", 2)
    map.put("description", 3)
    map.put("emailAddress", 4)
    map.put("country", 5)
    map.put("region", 6)
    map.put("city", 7)
    map.put("postalCode", 8)
    map.put("website", 9)
    map.put("image", 10)
    map
  }
}
