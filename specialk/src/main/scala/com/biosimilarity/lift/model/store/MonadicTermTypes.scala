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
extends MonadicGenerators with PrologMgr {
  type GetRequest = CnxnCtxtLabel[Namespace,Var,Tag]  
  trait Resource extends Serializable {
    def apply(
      pattern : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Option[CnxnCtxtLabel[Namespace,Var,Tag]] = Some( pattern )
  }
  trait RBound extends Resource { 
    def rsrc : Option[Resource]
    def soln : Option[Solution[Object]]
    def sbst : Option[LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]]]
    def assoc : Option[List[( Var, CnxnCtxtLabel[Namespace,Var,Tag] )]]

    override def apply( pattern : CnxnCtxtLabel[Namespace,Var,Tag] ) : Option[CnxnCtxtLabel[Namespace,Var,Tag]] = {
      def loop(
	ptn : CnxnCtxtLabel[Namespace,Var,Tag],
	hm : LinkedHashMap[Var, CnxnCtxtLabel[Namespace,Var,Tag]]
      ) : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
	ptn match {
	  case CnxnCtxtBranch( ns, branches ) => {
	    new CnxnCtxtBranch[Namespace,Var,Tag](
	      ns,
	      branches.map(
		( ccl : CnxnCtxtLabel[Namespace,Var,Tag] ) => {
		  loop( ccl, hm )
		}
	      )
	    )
	  }
	  case CnxnCtxtLeaf( Left( t ) ) => {
	    new CnxnCtxtLeaf[Namespace,Var,Tag]( Left( t ) )
	  }
	  case CnxnCtxtLeaf( Right( v ) ) => {
	    hm.get( v ) match {
	      case Some( ccl : CnxnCtxtLabel[Namespace,Var,Tag] with Factual ) => ccl
	      case None => {
		new CnxnCtxtLeaf[Namespace,Var,Tag]( Right( v ) )
	      }
	    }
	  }
	}
      }

      sbst match {
	case Some( hm ) => Some( loop( pattern, hm ) )
	case None => {
	  assoc match {
	    case Some( alst ) => {
	      val hm = new LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]]()	    
	      for( ( v, ccl ) <- alst ) { hm += ( v -> ccl ) }
	      Some( loop( pattern, hm ) )
	    }
	    case None => {
	      for( s <- soln; hm <- asHM( s, pattern ) ) yield { loop( pattern, hm ) }
	    }
	  }
	}
      }      
    }
  } 

  object RBound {
    def apply(
      rsrc : Option[Resource], soln : Option[Solution[Object]]
    ) : RBound = {
      RBoundP4JSoln( rsrc, soln )
    }
    def unapply(
      rsrc : RBoundP4JSoln
    ) : Option[( Option[Resource], Option[Solution[Object]] )] = {
      Some( ( rsrc.rsrc, rsrc.soln ) )
    }
    def unapply(
      rsrc : RBoundHM
    ) : Option[( Option[Resource], Option[LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]]] )] = {
      Some( ( rsrc.rsrc, rsrc.sbst ) )
    }
    def unapply(
      rsrc : RBoundAList
    ) : Option[( Option[Resource], Option[List[( Var, CnxnCtxtLabel[Namespace,Var,Tag] )]] )] = {
      Some( ( rsrc.rsrc, rsrc.assoc ) )
    }
  }
  case class Ground( v : Value ) extends Resource
  case class Cursor( v : Generator[Resource,Unit,Unit] ) extends Resource
  case class RMap(
    m : TMapR[Namespace,Var,Tag,Value]
  ) extends Resource
  case class RBoundP4JSoln(
    rsrc : Option[Resource], soln : Option[Solution[Object]]
  ) extends RBound {
    override def sbst : Option[LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]]] = None
    override def assoc : Option[List[( Var, CnxnCtxtLabel[Namespace,Var,Tag] )]] = None        
  }
  case class RBoundHM(
    rsrc : Option[Resource], sbst : Option[LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]]]
  ) extends RBound {
    override def soln : Option[Solution[Object]] = None
    override def assoc : Option[List[( Var, CnxnCtxtLabel[Namespace,Var,Tag] )]] = None
  }
  case class RBoundAList(
    rsrc : Option[Resource], assoc : Option[List[( Var, CnxnCtxtLabel[Namespace,Var,Tag] )]]
  ) extends RBound {
    override def soln : Option[Solution[Object]] = None
    override def sbst : Option[LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]]] = {
      assoc match {
	case Some( alist ) => {
	  val lhm = new LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]]()
	  for( ( v, t ) <- alist ) {
	    lhm += ( v -> t )
	  }
	  Some( lhm )
	}
	case _ => None
      }      
    }
  }

  object theCnxnTool
       extends CnxnUnificationTermQuery[Namespace,Var,Tag]
       with CnxnConversions[Namespace,Var,Tag]
       with UUIDOps
  implicit def cnxnTool : CnxnUnificationTermQuery[Namespace,Var,Tag] = {
    theCnxnTool
  }

  implicit def asHM(
    soln : Solution[Object], pattern : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : Option[LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]]] = {
    val pVars =
      ( ( Nil : List[Var] ) /: pattern.atoms )(
	( acc : List[Var], atom : Either[Tag,Var] ) => {
	  atom match {
	    case Right( v ) => acc ++ List( v )
	    case _ => acc
	  }
	}
      ).toSet

    pVars.isEmpty match {
      case false => {
	val hmSoln = new LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]]()
	for( v <- pVars ) {
	  try {
	    val solnTag : Solution[Object] = soln.on( "X" + v )
	    hmSoln += ( v -> cnxnTool.asCnxnCtxtLabel( solnTag.get ) )
	  }
	  catch {
	    case e : org.prolog4j.UnknownVariableException => {
//	      println( "warning: variable not bound: " + v )
	    }
	  }
	}
	Some( hmSoln )
      }
      case _ => None
    }
  }

  implicit def asRBoundHM(
    rsrc : RBoundP4JSoln,
    pattern : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : RBoundHM = {    
    RBoundHM(
      rsrc.rsrc,
      (for( s <- rsrc.soln ) yield { asHM( s, pattern ) }).flatMap( ( x ) => x )
    )    
  }

  implicit def asRBoundHM( rsrc : RBoundAList ) : RBoundHM = {
    RBoundHM( rsrc.rsrc, rsrc.sbst )
  }
  implicit def asRBoundAList( rsrc : RBoundHM ) : RBoundAList = {
    RBoundAList( rsrc.rsrc, for( map <- rsrc.sbst ) yield { map.toList } )
  }
  
  def portRsrc(
    rsrc : Resource, path : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : Resource = {
    rsrc match {
      case rsrcGnd : Ground => rsrc
      // BUGBUG -- lgm : Cursor might contain innerRsrc's
      case rsrcCrsr : Cursor => rsrc
      case rsrcBnd : RBound => {
	// BUGBUG -- lgm : we don't know that path applies to innerRsrc
	rsrcBnd match {
	  case RBoundP4JSoln( Some( innerRsrc ), optSoln ) => {
	    asRBoundAList(
	      asRBoundHM(
		RBoundP4JSoln( Some( portRsrc( innerRsrc, path ) ), optSoln ),
		path
	      )
	    )
	  }
	  case rsrcP4J@RBoundP4JSoln( None, optSoln ) => {
	    asRBoundAList( asRBoundHM( rsrcP4J, path ) )
	  }
	  case RBoundHM( Some( innerRsrc ), optSoln ) => {
	    asRBoundAList(
	      RBoundHM( Some( portRsrc( innerRsrc, path ) ), optSoln )
	    )
	  }
	  case rsrcHM@RBoundHM( None, optSoln ) => {
	    asRBoundAList( rsrcHM )
	  }
	  case rsrcAL : RBoundAList => {
	    rsrcAL
	  }
	}
      }
      case _ => {
	throw new Exception( "non-portable resource: " + rsrc )
      }
    }
  }    

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
