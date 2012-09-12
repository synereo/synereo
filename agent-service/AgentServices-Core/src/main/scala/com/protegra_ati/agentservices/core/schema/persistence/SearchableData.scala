package com.protegra_ati.agentservices.core.schema.persistence

import com.protegra_ati.agentservices.core.schema.Data
import com.protegra_ati.agentservices.core.schema.util.ReflectionHelper
import com.protegra_ati.agentservices.core.schema.Constants._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import java.lang.reflect._


/* User: mgevantmakher
*/

trait SearchableData
{
  self: Data =>

  /**
   * removes all ids and localeCode from the data object itself and all fields of the type Data recursively
   *
   */
  def prepareForSearch(): Unit =
  {
    this.id = ""
    this.localeCode = ""
    //    val fields = ReflectionHelper.getAllFields(this.getClass)
    //    fields.foreach(f => handleDataField(f))
  }

  // builds the search key, for this object to be used for search like a search command repository
  def toSearchKey: String =
  {
    val searchValueList = this.getSearchValueList()
    this.formattedClassName + "(" + searchValueList + ")"
  }

  def toSearchKeyFromClass(classType: Class[ _ <: Data ]): String =
  {
    formattedClassName(classType) + "(" + SEARCH_ANY + ")"
  }

  def getSearchFormattedFieldValue(f: Field): String =
  {
    var formatted = getFormattedFieldValue(f)
    //if ( classOf[ Data ].isAssignableFrom(f.getType) )
    //    formatted = formatted.replaceAll(EMPTY_LIST_TO_STRING, SEARCH_ANY)
    formatted match {
      case "\"\"" => SEARCH_ANY
      case _ => formatted
    }
  }


  protected def getSearchValueList(): String =
  {

    val fields = ReflectionHelper.getAllFields(this.getClass)
    val searchParameters = fields map ( f => handleFieldValue(f) /*f.getName().toCamelCase + "(" + getSearchFormattedFieldValue(f) + ")"*/ )

    searchParameters.isEmpty match {
      case true => {
        SEARCH_ANY
      }
      case false => {
        val onlyValues = fields map ( f => getFormattedFieldValue(f) )

        if ( containsOnlyAcceptedStrings(onlyValues, SEARCH_ANY) )
          SEARCH_ANY
        else if ( containsOnlyAcceptedStrings(onlyValues, "\"\"") )
          SEARCH_ANY
        else
          FIELDS + "(" + searchParameters.mkString("", ",", "") + ")"
      }
    }

  }

  private def handleDataField(f: Field): Unit =
  {
    if ( classOf[ Data ].isAssignableFrom(f.getType) ) {
      f.setAccessible(true)
      val fieldValue: Object = f.get(this)
      if ( fieldValue != null )
        fieldValue.asInstanceOf[ Data ].prepareForSearch()
    }
  }

  private def handleFieldValue(f: Field): String =
  {
    val trimmedfieldName = f.getName.trimPackage.toCamelCase
    val ignored = ignoredFieldsForSearchAndStoreKey.contains(trimmedfieldName)
    var content: String = ""

    if ( classOf[ Data ].isAssignableFrom(f.getType) ) {
      f.setAccessible(true)
      val fieldValue: Object = f.get(this)

      if ( fieldValue == null || ignored )
        content = SEARCH_ANY
      else {
        content = fieldValue.asInstanceOf[ Data ].toSearchKey
      }
    }
    else if ( classOf[ java.util.List[ String ] ].isAssignableFrom(f.getType) ) {
      f.setAccessible(true)
      val listValue: java.util.List[ String ] = f.get(this).asInstanceOf[ java.util.List[ String ] ]

      if ( listValue == null || listValue == Nil || listValue.isEmpty() || ignored )
        content = SEARCH_ANY
      else
        content = getSearchFormattedFieldValue(f)
    }
    else if ( classOf[ java.util.HashMap[ String, Data ] ].isAssignableFrom(f.getType) ) {
      //skip if it's hashmap for now
      content = SEARCH_ANY
    }
    else {
      if ( !ignored )
        content = getSearchFormattedFieldValue(f)
      else content = SEARCH_ANY
    }

    trimmedfieldName + "(" + content + ")"
  }

  private def isFieldNull(field: Field): Boolean =
  {
    field.setAccessible(true)
    field.get(this) == null
  }

  // evaluates a list of strings regarding containing strings. Returns true i
  private def containsOnlyAcceptedStrings(strings: List[ String ], acceptedString: String): Boolean =
  {
    val containsOnlyAccepted = strings.exists(_.equals(acceptedString) == false)
    !containsOnlyAccepted
  }

}