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
import scala.collection.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer

import org.prolog4j._

class ExternalConditionsMap[V](
  override val self : HashMap[String,V]
) extends MapProxy[String,V]

object ExternalConditions extends Serializable {
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

  case class Specimen(
    originalType : reflect.runtime.universe.Type,
    generatingExpression : Option[ExprTree],
    contents : AnyRef
  )

  var _exCndMap : Option[ExternalConditionsMap[Specimen]] = None
  def exCndMap() : ExternalConditionsMap[Specimen] = {
    _exCndMap match {
      case Some( ecm ) => ecm
      case None => {
        val ecm = new ExternalConditionsMap[Specimen]( new HashMap[String,Specimen]() )
        _exCndMap = Some( ecm )
        ecm
      }
    }
  }
}
