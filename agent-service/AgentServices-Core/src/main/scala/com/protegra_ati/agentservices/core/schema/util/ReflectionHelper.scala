package com.protegra_ati.agentservices.core.schema.util

import java.text._
import java.lang.reflect._
import scala.collection._

import scala.annotation.Annotation
import com.protegra_ati.agentservices.core.schema.persistence.ExcludedFields

object ReflectionHelper
{
  // TODO proper exception handling for all of the reflection calls, to prevent java RuntimeException

  final val NUMBER_TYPES = List("int", "short", "long", "float", "double")
  final val BOOLEAN_TYPE = "boolean"
  final val DATE_TYPE = "java.util.Date"
  final val SEARCH_ANY = "_"
  final val TRAIT_DELIMITER = "$$"

  //SimpleDateFormat causes massive (120KB vs 1KB) bloat in serialized classes, keep it in isolated class
  final val DATE_FORMATTER = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss")

  def getAllFields(classType: Class[ _ <: Any ]): List[ Field ] =
  {
   getAllFields(classType, ExcludedFields.excludedFields)
  }

  private def getAllFields(classType: Class[ _ <: Any ], excludedFieldNames: List[ String ]): List[ Field ] =
  {
    classType match {
      case null => Nil
      case _ => getAllFields(classType.getSuperclass) ::: ( classType.getDeclaredFields().toList.filter(x => !excludedFieldNames.contains(x.getName)) )
    }
  }

  def getAllFieldsNames(classType: Class[ _ <: Any ]): List[ String ] =
  {
   // getAllFieldsNames(classType, true)
    getAllFieldsNames(classType, true,ExcludedFields.excludedFields)
  }

  private def getAllFieldsNames(classType: Class[ _ <: Any ], excludedFieldNames: List[ String ]): List[ String ] =
  {
    getAllFieldsNames(classType, true, excludedFieldNames)
  }

  def getAllFieldsNames(classType: Class[ _ <: Any ], superclassIncluded: Boolean): List[ String ] =
  {
    classType match {
      case null => Nil
      case _ => ( if ( superclassIncluded == true ) getAllFieldsNames(classType.getSuperclass) ::: ( classType.getDeclaredFields().toList map ( f => f.getName ) )
      else classType.getDeclaredFields().toList map ( f => f.getName ) )
    }
  }


 private def getAllFieldsNames(classType: Class[ _ <: Any ], superclassIncluded: Boolean, excludedFieldNames: List[ String ]): List[ String ] =
  {
    classType match {
      case null => Nil
      case _ => ( if ( superclassIncluded == true ) getAllFieldsNames(classType.getSuperclass, excludedFieldNames) ::: ( ( classType.getDeclaredFields().toList map ( f => f.getName ) ).filter(x => !excludedFieldNames.contains(x)) )
      else ( classType.getDeclaredFields().toList map ( f => f.getName ) ).filter(x => !excludedFieldNames.contains(x)) )
    }
  }


  def getFieldValue(fieldName: String, fromObject: Object): Object =
  {
    val f = fromObject.getClass.getDeclaredField(fieldName)
    f.setAccessible(true)
    val fieldValue = f.get(fromObject)
    fieldValue
  }

  def getFieldName(name: String): String = {
    val indexStart = name.indexOf(TRAIT_DELIMITER)
    if (indexStart >= 0)
      name.substring(indexStart + TRAIT_DELIMITER.length)
    else
      name
  }

}