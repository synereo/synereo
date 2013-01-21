package com.protegra_ati.agentservices.core.schema


import java.io.Serializable
import java.lang.reflect._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.core.schema.util.ReflectionHelper._
import scala.reflect.BeanProperty
import com.protegra_ati.agentservices.core.schema.util._
import com.protegra_ati.agentservices.core.messages._
import com.protegra_ati.agentservices.store.extensions._
import com.protegra_ati.agentservices.core.schema.Constants._
import com.protegra_ati.agentservices.store.schema.KVDBSerializable
import java.util.{Locale, UUID, HashMap}
import validator._
import persistence._
import com.protegra_ati.agentservices.core.util.serializer.{UseKryoSerialization}
import java.lang.{Integer}

//TODO: see if Data object on DataValidator constructor needs to be made more efficient
class Data(_id: String, _localeCode: String /*,_classVersionNumber: String*/) extends Serializable
with KVDBSerializable
with StorableDataDefaults
with StorableData
with SearchableData
with Validable
with DataValidator
with UseKryoSerialization
{
  //def this(_id: String, _localeCode: String) = this(_id, _localeCode, 1)

  def this() = this("" , "" /*, Data.currentVersion*/)

  @BeanProperty
  var id: String = _id

  @BeanProperty
  var localeCode: String = _localeCode

  @BeanProperty
  var recVerNum: String = ""
 // @BeanProperty
 // var classVersionNumber = _classVersionNumber

  //default implementation returns an empty map
  //TODO: The Displayable names should really come from a resource lookup by language
  protected def getDisplayableFieldNames: HashMap[ String, String ] =
  {
    new HashMap[ String, String ]()
  }


  protected def getDisplayableFieldSortOrder: HashMap[ String, Integer ] =
  {
    new HashMap[ String, Integer ]()
  }

  def authorizedFieldNames(authorizedContentItem: AuthorizedContentAuditItem): String =
  {
    val fields = ReflectionHelper.getAllFields(this.getClass)
    val authorizedFields = authorizedContentItem.fields.split(",")
    val map = getDisplayableFieldNames
    val displayableFields = for (
      field <- fields; if ( authorizedFields.contains(field.getName) ); df = map.get(field.getName); if df != null
    ) yield df

    displayableFields.mkString("", ",", "")
  }

  def getDisplayName(): String =
  {
    "Data"
  }

  //note:  this doesn't work with wildcards right now
  //  def toDeleteKey : String = {
  //    val fields = ReflectionHelper.getAllFields(this.getClass)
  //    val fieldValues = fields map (f => f.getName() + "(" + getFormattedFieldValue(f, true) + ")")
  //    val fieldValueList = fieldValues.mkString("", ",", "")
  //    this.formattedClassName + "(" + FIELDS + "(" + fieldValueList + "))"
  //  }

  def authorizedData(fields: String): Data =
  {
    val fieldList = fields.split(StringExtensions.separators).toList
    authorizedData(fieldList)
  }

  def authorizedData(authorizedFieldList: List[ String ]): Data =
  {
    //val className = stripPackage(this.getClass.getName)
    val fields = ReflectionHelper.getAllFields(this.getClass)
    val authorizedFields = fields.filter(f => authorizedFieldList.contains(ReflectionHelper.getFieldName(f.getName)))
    val dataClass = Class.forName(this.getClass.getName)
    var data = dataClass.newInstance
    authorizedFields.foreach(f => {f.setAccessible(true); f.set(data, f.get(this))})
    data.asInstanceOf[ Data ]
  }

  //if this is val it shows up on toStoreKey/toSearchKey
  def className: String =
  {
    this.getClass.getName
  }

  //if this is val it shows up on toStoreKey/toSearchKey
  //important to note that formattedClassName matches up to DisclosedData.dataDisplayClassName
  def formattedClassName: String =
  {
    this.getClass.getName.trimPackage.toCamelCase
  }

  def formattedClassName(classType: Class[ _ <: Data ]): String =
  {
    classType.getName.trimPackage.toCamelCase
  }

  def getValue(fieldName: String): String =
  {
    val field = this.getClass.getDeclaredField(fieldName)
    getFieldValue(field)
  }

  /**
   * Qutation marks will be included by default
   * @param field
   * @return
   */
  protected def getFormattedFieldValue(field: Field): String =
  {
    // the list can be extended if new complex types has to be excluded from quotation
    //exceptions of type data need to be first in the condition list
    //ie image
    if (ignoredFieldsForSearchAndStoreKey().contains(field.getType.toString.trimPackage.toCamelCase))  {
      "\"" + "\""
    } else if ( classOf[ Data ].isAssignableFrom(field.getType) )
      ( getFieldValue(field) )
    //TODO: this is too aggressive problems with hashmap, we need to fix to handle reflection better, likely similar to data
    else if ( classOf[ Message ].isAssignableFrom(field.getType) ) {
      //skip if it's message for now
      "\"" + "\""
    }
    else ( "\"" + getFieldValue(field) + "\"" )
  }

  protected def getFormattedFieldValue(field: Field, quotationIncluded: Boolean): String =
  {
    if ( quotationIncluded == true ) getFormattedFieldValue(field)
    else ( getFieldValue(field) )
  }

  def getFieldValue(field: Field): String =
  {
    field.setAccessible(true)
    // TODO such way of type name extraction has to be changed it is just declared type
    // TODO if something like List l = new MySuperExtendedList (...) is given it will returns just List
    val fieldType = field.getType.toString.replace("class ", "")
    val fieldValue: Object = field.get(this)

    if ( fieldValue == null )
      ""
    else {
      if ( fieldType == DATE_TYPE )
        formatDateValue(fieldValue)
      /*else if (isInstanceOfData(fieldValue)){
        fieldValue.asInstanceOf[Data].getDataValue()
      } */
      else
        fieldValue.toString
    }
  }

  private def formatDateValue(dateFieldValue: Object): String =
  {
    if ( dateFieldValue == null ) {""}
    else DATE_FORMATTER.format(dateFieldValue)
  }

  private def isInstanceOfData(someFieldValue: Object): Boolean =
  {
    if ( someFieldValue.isInstanceOf[ Data ] )
      true
    else
      false
  }


  def getDataValue(): String =
  {
    val fields = ReflectionHelper.getAllFields(this.getClass)
    val fieldValues = fields map ( f => f.getName().toCamelCase + "(" + getFormattedFieldValue(f) + ")" )
    val fieldValueList = fieldValues.mkString("", ",", "")
    this.formattedClassName + "(" + FIELDS + "(" + fieldValueList + "))"
  }

  protected def ignoredFieldsForSearchAndStoreKey(): List[ String ] =
  {
    Nil
  }
  protected def keyFieldsForSearchAndStoreKey(): List [ String ] =
  {
    var keyList : List[String] = Nil
    keyList = "id" :: keyList
    keyList = "localeCode" :: keyList
    keyList = "recVerNum" :: keyList
    keyList

  }


  //FYI:
  //To get a list of field names -  val fieldNames = fields map {"\"" + _.getName + "\""}
  //To instantiate a class - val myClass = Class.forName(this.getClass.getName)
}

object Data
{
  def currentVersion: String = "1.0"
}
