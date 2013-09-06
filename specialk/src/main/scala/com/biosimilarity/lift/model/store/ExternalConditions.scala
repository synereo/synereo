// -*- mode: Scala;-*- 
// Filename:    ExternalConditions.scala 
// Authors:     lgm                                                    
// Creation:    Thu Sep  5 12:04:16 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store
import com.biosimilarity.lift.lib._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.util.continuations._ 
import scala.xml._
import scala.collection.mutable.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer

import org.prolog4j._

import java.util.UUID

class ExternalConditionsMap[V](
  override val self : HashMap[String,V]
) extends MapProxy[String,V]

trait ReflectiveSurfaceT {
  import scala.reflect.runtime.universe._
  import scala.tools.reflect.ToolBox
  import scala.reflect.runtime.{currentMirror => cMr}

  type ToolBelt = scala.tools.reflect.ToolBox[reflect.runtime.universe.type]
  
  var _toolBox : Option[ToolBelt] = None
  def toolBox() : ToolBelt = {
    _toolBox match {
      case Some( tb ) => tb
      case None => {
        val tb = cMr.mkToolBox()
        _toolBox = Some( tb )
        tb
      }
    }
  }

  lazy val tb : ToolBelt = toolBox()

  type ExprTree = tb.u.Tree

  def getType[T: TypeTag](obj: T) = typeOf[T]

  case class Specimen(
    originalType : reflect.runtime.universe.Type,
    generatingExpression : Option[ExprTree],
    contents : AnyRef
  )  
}

trait ExternalConditionsT extends Serializable
with ReflectiveSurfaceT
with MapProxy[String,ReflectiveSurfaceT#Specimen]
{
  import scala.reflect.runtime.universe._

  override def self = exCndMap.self
  var _exCndMap : Option[ExternalConditionsMap[ReflectiveSurfaceT#Specimen]] = None
  def exCndMap() : ExternalConditionsMap[ReflectiveSurfaceT#Specimen] = {
    _exCndMap match {
      case Some( ecm ) => ecm
      case None => {
        val ecm = new ExternalConditionsMap[ReflectiveSurfaceT#Specimen]( new HashMap[String,ReflectiveSurfaceT#Specimen]() )
        _exCndMap = Some( ecm )
        ecm
      }
    }
  }  

  implicit def captureSpecimen( content : AnyRef ) : Specimen = {
    Specimen( getType( content ), None, content )
  }

  def captureSpecimen(
    content : AnyRef,
    genExpr : ReflectiveSurfaceT#ExprTree
  ) : Specimen = {
    Specimen( getType( content ), Some( genExpr ), content )
  }

  def registerContent( content : AnyRef ) : String = {
    val uuidStr = UUID.randomUUID.toString
    exCndMap += ( uuidStr -> content )
    uuidStr
  }
  def registerContent( tag : String, content : AnyRef ) : Unit = {
    exCndMap += ( tag -> content )
  }
  def retrieveContent[T : TypeTag](
    tag : String,
    treatCastFailureSilently : Boolean = true
  ) : Option[T] = {
    try {      
      for( specimen <- exCndMap.get( tag ) ) yield {      
        val t = typeOf[T]
        if ( specimen.originalType <:< t ) {
          specimen.contents.asInstanceOf[T]
        }
        else {
          BasicLogService.tweet(
            "retrieveContent failed to verify type constraint"
            + "\nspecimen.originalType: " + specimen.originalType
            + "\nrequested type: " + t
          )
          throw new java.lang.ClassCastException()
        }
      }
    }
    catch {
      case e : java.lang.ClassCastException => {
        if ( treatCastFailureSilently ) {
          None
        }
        else {
          throw e
        }
      }
    }
  }
}

object ExternalConditions extends ExternalConditionsT

