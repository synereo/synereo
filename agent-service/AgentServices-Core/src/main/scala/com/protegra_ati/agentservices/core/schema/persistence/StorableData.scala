package com.protegra_ati.agentservices.core.schema.persistence

import com.protegra_ati.agentservices.core.schema.Data
import com.protegra_ati.agentservices.core.schema.util.{PrologFormatter, ReflectionHelper}
import com.protegra_ati.agentservices.core.schema.Constants._
import com.protegra.agentservicesstore.extensions.StringExtensions._
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
  private def toStoreKey(isChild: Boolean): String =
  {
    // presets defaults if required
    this.setDefaultValues(isChild)

    // TODO here should the validation happen, throw the exception to mark illegal values
    val fields = ReflectionHelper.getAllFields(this.getClass)
    val fieldValues = fields map ( f => handleFieldValue(f) /*f.getName().toCamelCase + "(" + this.getFormattedFieldValue(f) + ")" */ )
    val fieldValueList = fieldValues.mkString("", ",", "")
    this.formattedClassName + "(" + FIELDS + "(" + PrologFormatter.clean(fieldValueList) + "))"
  }

  def toDeleteKey(): String =
  {
    //TODO: this can be streamlined once we know we have class(keys(),fields(_))
    val fields = ReflectionHelper.getAllFields(this.getClass)
    val fieldValues = fields map ( f => handleDeleteFieldValue(f) /*f.getName().toCamelCase + "(" + this.getFormattedFieldValue(f) + ")" */ )
    val fieldValueList = fieldValues.mkString("", ",", "")
    this.formattedClassName + "(" + FIELDS + "(" + PrologFormatter.clean(fieldValueList) + "))"
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
    else if ( classOf[ java.util.HashMap[ String, Data ] ].isAssignableFrom(f.getType) ) {
      //skip if it's hashmap for now
      content = "\"" + "\""
    }
    //TODO: problems with hashmap, we need to fix to handle reflection better, likely similar to data
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
    else if ( classOf[ java.util.HashMap[ String, Data ] ].isAssignableFrom(f.getType) ) {
      //skip if it's hashmap for now
      content = ""
    }
    //TODO: problems with hashmap, we need to fix to handle reflection better, likely similar to data
    else if ( classOf[ Message ].isAssignableFrom(f.getType) ) {
      //skip if it's message for now
      content = ""
    }
    else {
      content = this.getFieldValue(f)
      prop.setProperty(parentElementName + "." + f.getName.trimPackage.toCamelCase, content)
    }

  }

}