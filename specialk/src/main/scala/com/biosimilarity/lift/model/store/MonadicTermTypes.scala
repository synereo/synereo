// -*- mode: Scala;-*- 
// Filename:    MonadicTermTypes.scala 
// Authors:     lgm                                                    
// Creation:    Mon Feb 27 19:07:30 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._

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

//import org.exist.storage.DBBroker

import org.xmldb.api.base.{ Resource => XmlDbRrsc, _}
import org.xmldb.api.modules._
import org.xmldb.api._

//import org.exist.util.serializer.SAXSerializer
//import org.exist.util.serializer.SerializerPool

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import javax.xml.transform.OutputKeys
import java.util.Properties
import java.net.URI
import java.io.File
import java.io.FileInputStream
import java.io.OutputStreamWriter

trait MonadicTermTypes[Namespace,Var,Tag,Value] 
extends MonadicGenerators {
  trait Resource extends Serializable
  trait RBound extends Resource {
    def rsrc : Option[Resource]
    def soln : Option[Solution[String]]
    def sbst : Option[HashMap[Var,Tag]]
  } 
  object RBound {
    def apply(
      rsrc : Option[Resource], soln : Option[Solution[String]]
    ) : RBound = {
      RBoundP4JSoln( rsrc, soln )
    }
    def unapply(
      rsrc : RBoundP4JSoln
    ) : Option[( Option[Resource], Option[Solution[String]] )] = {
      Some( ( rsrc.rsrc, rsrc.soln ) )
    }
    def unapply(
      rsrc : RBoundHM
    ) : Option[( Option[Resource], Option[HashMap[Var,Tag]] )] = {
      Some( ( rsrc.rsrc, rsrc.sbst ) )
    }
  }
  case class Ground( v : Value ) extends Resource
  case class Cursor( v : Generator[Resource,Unit,Unit] ) extends Resource
  case class RMap(
    m : TMapR[Namespace,Var,Tag,Value]
  ) extends Resource
  case class RBoundP4JSoln(
    rsrc : Option[Resource], soln : Option[Solution[String]]
  ) extends RBound {
    override def sbst : Option[HashMap[Var,Tag]] = None
  }
  case class RBoundHM(
    rsrc : Option[Resource], sbst : Option[HashMap[Var,Tag]]
  ) extends RBound {
    override def soln : Option[Solution[String]] = None
  }

  type GetRequest = CnxnCtxtLabel[Namespace,Var,Tag]  

  class TMapR[Namespace,Var,Tag,Value]
  extends HashMap[GetRequest,Resource]  

  case class Continuation(
    ks : List[Option[Resource] => Unit @suspendable]
  ) extends Resource

}

trait MonadicTermTypeScope[Namespace,Var,Tag,Value] {
  type MTTypes <: MonadicTermTypes[Namespace,Var,Tag,Value]
  def protoTermTypes : MTTypes
  val mTT : MTTypes = protoTermTypes
  def asCCL(
    gReq : mTT.GetRequest
  ) : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
    gReq.asInstanceOf[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
  }
  implicit def toValue( v : Value ) = mTT.Ground( v )
}
