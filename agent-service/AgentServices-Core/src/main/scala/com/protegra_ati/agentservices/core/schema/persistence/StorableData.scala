package com.protegra_ati.agentservices.core.schema.persistence

import com.protegra_ati.agentservices.core.schema.Data
import com.protegra_ati.agentservices.core.schema.util.{PrologFormatter, ReflectionHelper}
import com.protegra_ati.agentservices.core.schema.Constants._
import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import java.util.{Locale, UUID}
import java.util.Properties
import java.lang.reflect._
import com.protegra_ati.agentservices.core.messages.Message

trait StorableData extends StorableDataDefaults
{
  self: Data =>

  //important to note that toStoreKey (namely formattedClassName) matches up with DisclosedData.dataDisplayClassName
  def toStoreKey: String =
  {
    toStoreKey(false)
  }

  def toStoreKey(isChild: Boolean): String =
  {
    // presets defaults if required
    this.setDefaultValues(isChild)

    // TODO here should the validation happen, throw the exception to mark illegal values
    // setting up the "header" portion of the key

    val fields = ReflectionHelper.getAllFields(this.getClass)
    val filteredFields = fields.filter(r => !keyFieldsForSearchAndStoreKey.contains(r.getName.trimPackage.toCamelCase))
    val fieldValues = filteredFields collect {case f:Field if !ignoredFieldsForSearchAndStoreKey.contains(f.getName.trimPackage.toCamelCase) => handleFieldValue(f)}
    val fieldValueList = fieldValues.mkString("", ",", "")
    "data(" + this.formattedClassName + "(" + PrologFormatter.clean(getHeaderContent()) + "," + FIELDS + "(" + PrologFormatter.clean(fieldValueList) + ")))"
  }

  def toDeleteKey(): String =
  {
    //we will only be deleting by key
    keyByIdOnly()
  }

  private def handleDeleteFieldValue(f: Field): String =
  {
    val trimmedfieldName = f.getName.trimPackage.toCamelCase
    var content: String = ""

    if ( trimmedfieldName.equals("id") ) {content = this.getFormattedFieldValue(f)}
    else {content = SEARCH_ANY}

    trimmedfieldName + "(" + content + ")"
  }

  private def handleFieldValue(f: Field): String =
  {
    val trimmedfieldName = f.getName.trimPackage.toCamelCase
    val ignored = ignoredFieldsForSearchAndStoreKey.contains(trimmedfieldName)
   // val isKeyField = keyFieldsForSearchAndStoreKey.contains(trimmedfieldName)
    var content: String = ""
    if ( classOf[ Data ].isAssignableFrom(f.getType) ) {
      f.setAccessible(true)
      val fieldValue: Object = f.get(this)
      if ( fieldValue == null || ignored ) {
        content = "\"" + "\""
      }
      else {
        content = fieldValue.asInstanceOf[ Data ].toStoreKey(true)
      }
    }
    //TODO: problems with hashmap, we need to fix to handle reflection better, likely similar to data
    else if ( classOf[ java.util.HashMap[ String, Data ] ].isAssignableFrom(f.getType) ) {
      //skip if it's hashmap for now
      content = "\"" + "\""
    }
    else if ( classOf[ java.util.concurrent.ConcurrentHashMap[ Any, Any ] ].isAssignableFrom(f.getType) ) {
      //skip if it's concurrent hashmap for now
      content = "\"" + "\""
    }
    else if ( classOf[ Message ].isAssignableFrom(f.getType) ) {
      //skip if it's message for now
      content = "\"" + "\""
    }
    else {

      if ( !ignored ) {content = this.getFormattedFieldValue(f)}
      else {content = "\"" + "\""}
    }

    //TODO: centralize this so both storable and searchable use it
    trimmedfieldName + "(" + content + ")"

  }


  def toProperty(result: Properties): Unit =
  {
    toProperty(result, this.formattedClassName)
  }

  private def toProperty(result: Properties, parentElementName: String): Unit =
  {
    val fields = ReflectionHelper.getAllFields(this.getClass)

    fields.foreach(f => fieldValueToProperty(f, parentElementName, result))
  }


  private def fieldValueToProperty(f: Field, parentElementName: String, prop: Properties): Unit =
  {
    var content: String = ""
    if ( classOf[ Data ].isAssignableFrom(f.getType) ) {
      f.setAccessible(true)
      val fieldValue: Object = f.get(this)
      if ( fieldValue == null ) {
        // do nothing
      }
      else {
        fieldValue.asInstanceOf[ Data ].toProperty(prop, parentElementName + "." + f.getName.trimPackage.toCamelCase)
      }
    }
      //TODO: problems with hashmap, we need to fix to handle reflection better, likely similar to data
    else if ( classOf[ java.util.HashMap[ String, Data ] ].isAssignableFrom(f.getType) ) {
      //skip if it's hashmap for now
      content = ""
    }
    else if ( classOf[ Message ].isAssignableFrom(f.getType) ) {
      //skip if it's message for now
      content = ""
    }
    else {
      content = this.getFieldValue(f)
      prop.setProperty(parentElementName + "." + f.getName.trimPackage.toCamelCase, content)
    }

  }
  def keyByIdOnly(): String =
  {
    "data(" + this.formattedClassName + "(" + KEYS + "(id(\"" + id + "\"),localeCode(_),recVerNum(_))," + "_))"
  }

  def getHeaderContent() : String =
  {
    val header = KEYS_TEMPLATE.replace("%ID", id).replace("%LOCALE_CODE", localeCode).replace("%REC_VER_NUM", recVerNum.toString)
    header
  }

}