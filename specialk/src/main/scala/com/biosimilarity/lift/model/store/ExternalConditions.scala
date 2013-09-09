// -*- mode: Scala;-*- 
// Filename:    ExternalConditions.scala 
// Authors:     lgm                                                    
// Creation:    Thu Sep  5 12:04:16 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store
import com.biosimilarity.lift.lib._

import scala.reflect.runtime.universe._
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

  implicit def captureSpecimenAsT[T : TypeTag]( content : T ) : Specimen = {
    Specimen( typeOf[T], None, content.asInstanceOf[AnyRef] )
  }

  def captureSpecimenAsT[T : TypeTag](
    content : T,
    genExpr : ReflectiveSurfaceT#ExprTree
  ) : Specimen = {
    Specimen( typeOf[T], Some( genExpr ), content )
  }

  def registerContent( content : AnyRef ) : String = {
    val uuidStr = UUID.randomUUID.toString
    exCndMap += ( uuidStr -> content )
    uuidStr
  }
  def registerContent( tag : String, content : AnyRef ) : Unit = {
    exCndMap += ( tag -> content )
  }
  def registerContentAsT[T : TypeTag]( content : T ) : String = {
    val uuidStr = UUID.randomUUID.toString
    exCndMap += ( uuidStr -> captureSpecimenAsT( content ) )
    uuidStr
  }
  def registerContentAsT[T : TypeTag]( tag : String, content : T ) : Unit = {
    exCndMap += ( tag -> captureSpecimenAsT( content ) )
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

package external.conditions.usage {
  trait Looky[N] {
    def b : Boolean
    def s : String
    def n : N
    def r : Option[Looky[N]]
  }
  class LookyLoo(
    override val b : Boolean,
    override val s : String,
    override val n : Int,
    override val r : Option[Looky[Int]]    
  ) extends Looky[Int]
  object LookyLoo {
    def apply(
      b : Boolean, s : String, n : Int, r : Option[Looky[Int]]
    ) : LookyLoo = {
      new LookyLoo( b, s, n, r )
    }
    def unapply(
      ll : LookyLoo
    ) : Option[( Boolean, String, Int, Option[Looky[Int]] )] = {
      Some( ( ll.b, ll.s, ll.n, ll.r ) )
    }
  }
  class LookyLooToo(
    override val b : Boolean,
    override val s : String,
    override val n : Double,
    override val r : Option[Looky[Double]]    
  ) extends Looky[Double]
  object LookyLooToo {
    def apply(
      b : Boolean, s : String, n : Double, r : Option[Looky[Double]]
    ) : LookyLooToo = {
      new LookyLooToo( b, s, n, r )
    }
    def unapply(
      ll : LookyLooToo
    ) : Option[( Boolean, String, Double, Option[Looky[Double]] )] = {
      Some( ( ll.b, ll.s, ll.n, ll.r ) )
    }
  }

  object TryOut {
    def addContent() : ( String, String ) = {
      val ctag1 =
        ExternalConditions.registerContentAsT[LookyLoo](
          new LookyLoo( true, "true", 1, None )
        )
      val ctag2 =
        ExternalConditions.registerContentAsT[LookyLooToo](
          new LookyLooToo( true, "true", 1.0, None )
        )
      ( ctag1, ctag2 )
    }
    def positiveTests(
    ) : ( LookyLoo, LookyLooToo, Looky[Int], Looky[Double] ) = {            
      val ( ctag1, ctag2 ) = addContent()
      val ll1 : LookyLoo =
        ExternalConditions.retrieveContent[LookyLoo](
          ctag1
        ).getOrElse( null )
      val ll2 = 
        ExternalConditions.retrieveContent[LookyLooToo](
          ctag2
        ).getOrElse( null )
      
      val ll3 =
        ExternalConditions.retrieveContent[Looky[Int]](
          ctag1
        ).getOrElse( null )
      val ll4 =
        ExternalConditions.retrieveContent[Looky[Double]](
          ctag2
        ).getOrElse( null )

      ( ll1, ll2, ll3, ll4 )
    }
    
    def negativeTests() = {
      val ( ctag1, ctag2 ) = addContent()
      val ll1 : LookyLoo =
        ExternalConditions.retrieveContent(
          ctag1
        ).getOrElse( null )
      val ll2 = 
        ExternalConditions.retrieveContent(
          ctag2
        ).getOrElse( null )
      
      val ll3 =
        ExternalConditions.retrieveContent[Looky[Double]](
          ctag1
        ).getOrElse( null )
      val ll4 =
        ExternalConditions.retrieveContent[Looky[Int]](
          ctag2
        ).getOrElse( null )
      
      ( ll1, ll2, ll3, ll4 )
    }
  }
}
