// -*- mode: Scala;-*- 
// Filename:    SelfMonitoring.scala 
// Authors:     lgm                                                    
// Creation:    Thu Jun 23 15:49:34 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.monitor

import com.biosimilarity.lift.model.ApplicationDefaults

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.lib.monad._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.util.continuations._ 
import scala.xml._
import scala.collection.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MutableList

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

object Severity extends Enumeration()
{
  type Severity = Value
  val Fatal, Error, Warning, Info, Debug, Trace = Value
}

trait SeverityOps {
  def SeverityFromOption( level : Option[ String ] ) : Severity.Value =
  {
    level match {
      case Some( x ) => {
        SeverityFromString( x )
      }
      case None => {
        Severity.Debug
      }
    }
  }

  def SeverityFromString( level : String ) : Severity.Value =
  {
    level.toLowerCase() match {
      case "fatal" => {
        Severity.Fatal
      }
      case "error" => {
        Severity.Error
      }
      case "warning" => {
        Severity.Warning
      }
      case "info" => {
        Severity.Info
      }
      case "debug" => {
        Severity.Debug
      }
      case "trace" => {
        Severity.Trace
      }
      case _ => {
        Severity.Debug
      }
    }
  }
}

trait CnxnBasedLoggingScope[MW[_],M[_],Namespace,Var,Tag,Value]
//extends HistoricalContextScope[M,CnxnCtxtLabel[Namespace,Var,Tag]]
extends HistoricalContextScope[M,java.io.Serializable]
with PersistedTermStoreScope[Namespace,Var,Tag,Value]
with UUIDOps 
with SeverityOps {
  import SpecialKURIDefaults._
  import CnxnLeafAndBranch._
  import identityConversions._    

  object MonitorScope
    extends PersistedTermStoreScope[String,String,String,String] {
      type MTTypes = MonadicTermTypes[String,String,String,String]
      object TheMTT extends MTTypes
      override def protoTermTypes : MTTypes = TheMTT
      
      type DATypes = DistributedAskTypes
      object TheDAT extends DATypes
      override def protoAskTypes : DATypes = TheDAT
      
      class PersistedtedStringMGJ(
	val dfStoreUnitStr : String,
	//override val name : URI,
	override val name : Moniker,
	//override val acquaintances : Seq[URI]
	override val acquaintances : Seq[Moniker]
      ) extends PersistedMonadicGeneratorJunction(
	name, acquaintances
      ) {
	class StringXMLDBManifest(
	  override val storeUnitStr : String,
	  override val labelToNS : Option[String => String],
	  override val textToVar : Option[String => String],
	  override val textToTag : Option[String => String]        
	)
	extends XMLDBManifest( database ) {
	  override def storeUnitStr[Src,Label,Trgt](
	    cnxn : Cnxn[Src,Label,Trgt]
	  ) : String = {     
	    cnxn match {
	      case CCnxn( s, l, t ) =>
		s.toString + l.toString + t.toString
	    }
	  }	
	  
	  def kvNameSpace : String = "record"
	  
	  // BUGBUG -- LGM: Evidence of a problem with this factorization
	  override def asCacheValue(
	    ltns : String => String,
	    ttv : String => String,
	    value : Elem
	  ) : Option[String] = {
	    tweet(
	      "Shouldn't be here!"
	    )
	    None
	  }
	  
	  override def asStoreValue(
	    rsrc : mTT.Resource
	  ) : CnxnCtxtLeaf[String,String,String] with Factual = {
	    valueStorageType match {
	      case "CnxnCtxtLabel" => {
		tweet(
		  "warning: CnxnCtxtLabel method is using XStream"
		)
		
		val blob = toXQSafeJSONBlob( rsrc )
		
		new CnxnCtxtLeaf[String,String,String](
		  Left[String,String](
		    blob
		  )
		)
	      }
	      case "XStream" => {
		tweet(
		  "using XStream method"
		)
		
		val blob = toXQSafeJSONBlob( rsrc )
		
		//asXML( rsrc )
		new CnxnCtxtLeaf[String,String,String](
		  Left[String,String]( blob )
		)
	      }
	      case _ => {
		throw new Exception( "unexpected value storage type" )
	      }
	    }	  
	  }
	  
	  def asCacheValue(
	    ccl : CnxnCtxtLabel[String,String,String]
	  ) : String = {
	    tweet(
	      "converting to cache value"
	    )
	    //asPatternString( ccl )
	    ccl match {
	      case CnxnCtxtBranch(
		"String",
		CnxnCtxtLeaf( Left( rv ) ) :: Nil
	      ) => {
		val unBlob =
		  fromXQSafeJSONBlob( rv )
		
		unBlob match {
		  case rsrc : mTT.Resource => {
		    getGV( rsrc ).getOrElse( "" )
		  }
		}
	      }
	      case _ => {
		asPatternString( ccl )
	      }
	    }
	  }
	  
	}
	
	def persistenceManifest : Option[PersistenceManifest] = {
	  val sid = Some( ( s : String ) => s )
	  Some(
	    new StringXMLDBManifest( dfStoreUnitStr, sid, sid, sid )
	  )
	}
      }
      
      def ptToPt( storeUnitStr : String, a : String, b : String )  = {
	new PersistedtedStringMGJ( storeUnitStr, a, List( b ) )
      }
      
      def loopBack( storeUnitStr : String ) = {
	ptToPt( storeUnitStr, "localhost", "localhost" )
      }
      
      import scala.collection.immutable.IndexedSeq
      
      type MsgTypes = DTSMSH[String,String,String,String]   
      
      val protoDreqUUID = getUUID()
      val protoDrspUUID = getUUID()    
      
      object MonadicDMsgs extends MsgTypes {
	
	override def protoDreq : DReq = MDGetRequest( aLabel )
	override def protoDrsp : DRsp = MDGetResponse( aLabel, aLabel.toString )
	override def protoJtsreq : JTSReq =
	  JustifiedRequest(
	    protoDreqUUID,
	    new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	    new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	    getUUID(),
	    protoDreq,
	    None
	  )
	override def protoJtsrsp : JTSRsp = 
	  JustifiedResponse(
	    protoDreqUUID,
	    new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
	    new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
	    getUUID(),
	    protoDrsp,
	    None
	  )
	override def protoJtsreqorrsp : JTSReqOrRsp =
	  Left( protoJtsreq )
      }
      
      override def protoMsgs : MsgTypes = MonadicDMsgs
    }

  trait CnxnWriter[A] extends WriterM[A] {    
    abstract class AbstractWrapper[A](
      override val value : A,
      override val record : M[java.io.Serializable]
    ) extends MHCtxt[A]
    
    implicit def toMHCtxt[A](
      tpl : ( A, M[java.io.Serializable] )
    ) : MHCtxt[A]
    implicit def fromMHCtxt[A](
      mhctxt : MHCtxt[A]
    ) : ( A, M[java.io.Serializable] )

    def tweet( fact : java.io.Serializable ) : M[Unit]
    def blog( fact : java.io.Serializable ) : M[Unit]
    def report( fact : java.io.Serializable ) : M[Unit]    
    def record( fact : java.io.Serializable ) : M[Unit]
    def log( fact : java.io.Serializable ) : M[Unit]

    def tweetValue[A]( a : A ) : AbstractWrapper[A]
    def blogValue[A]( a : A ) : AbstractWrapper[A]
    def reportValue[A]( a : A ) : AbstractWrapper[A]
    def recordValue[A]( a : A ) : AbstractWrapper[A]
    def logValue[A]( a : A ) : AbstractWrapper[A]

    def tweet(
      fact : java.io.Serializable,
      severity : Severity.Value
    ) : M[Unit]
    def blog(
      fact : java.io.Serializable,
      severity : Severity.Value
    ) : M[Unit]
    def report(
      fact : java.io.Serializable,
      severity : Severity.Value
    ) : M[Unit]    
    def record(
      fact : java.io.Serializable,
      severity : Severity.Value
    ) : M[Unit]    
    def log(
      fact : java.io.Serializable,
      severity : Severity.Value
    ) : M[Unit]        

    def tweetValue[A]( a : A, severity : Severity.Value ) : AbstractWrapper[A]
    def blogValue[A]( a : A, severity : Severity.Value ) : AbstractWrapper[A]
    def reportValue[A]( a : A, severity : Severity.Value ) : AbstractWrapper[A]
    def recordValue[A]( a : A, severity : Severity.Value ) : AbstractWrapper[A]
    def logValue[A]( a : A, severity : Severity.Value ) : AbstractWrapper[A]
  }

  def beginLogging[A]( a : A ) : CnxnWriter[A]
  def endLogging[A]( cnxnWrtr : CnxnWriter[A] ) : Unit
}
