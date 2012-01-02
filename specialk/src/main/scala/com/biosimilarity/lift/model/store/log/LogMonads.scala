// -*- mode: Scala;-*- 
// Filename:    LogMonads.scala 
// Authors:     lgm                                                    
// Creation:    Fri Jul  1 10:53:28 2011 
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


class SimpleStoreScope[A]( )
    extends PersistedTermStoreScope[String,String,String,A] {
      import SpecialKURIDefaults._
      import CnxnLeafAndBranch._
      import identityConversions._    

      type MTTypes = MonadicTermTypes[String,String,String,A]
      object TheMTT extends MTTypes
      override def protoTermTypes : MTTypes = TheMTT
      
      type DATypes = DistributedAskTypes
      object TheDAT extends DATypes
      override def protoAskTypes : DATypes = TheDAT

      object Being extends PersistenceScope {

	override type EMTypes =
	  ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]

      object theEMTypes
      extends ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]
	{
	case class PrologSubstitution( soln : Solution[String] )
	   extends Function1[mTT.Resource,Option[mTT.Resource]] {
	     override def apply( rsrc : mTT.Resource ) = {
	       Some( mTT.RBound( Some( rsrc ), Some( soln ) ) )
	     }
	   }
	override type Substitution = PrologSubstitution
      }      

      override def protoEMTypes : EMTypes =
	theEMTypes
      
	class PersistedStringMGJ(
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
	    override def valueStorageType : String = {
	      throw new Exception( "valueStorageType not overriden in instantiation" )
	    }
	    override def continuationStorageType : String = {
	      throw new Exception( "continuationStorageType not overriden in instantiation" )
	    }

	    override def asResource(
	      key : mTT.GetRequest, // must have the pattern to determine bindings
	      value : Elem
	    ) : emT.PlaceInstance = {
	      throw new Exception( "not yet implemented" )
	    }
	    override def storeUnitStr[Src,Label,Trgt](
	      cnxn : Cnxn[Src,Label,Trgt]
	    ) : String = {     
	      cnxn match {
		case CCnxn( s, l, t ) =>
		  s.toString + l.toString + t.toString
	      }
	    }	
	    
	    def kvNameSpace : String = "record"
	    def kvKNameSpace : String = "kRecord"
	    
	    def compareNameSpace( ns1 : String, ns2 : String ) : Boolean = {
	      ns1.equals( ns2 )
	    }
	    
	    // BUGBUG -- LGM: Evidence of a problem with this factorization
	    override def asCacheValue(
	      ltns : String => String,
	      ttv : String => String,
	      value : Elem
	    ) : Option[A] = {
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
	    ) : A = {
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
		    fromXQSafeJSONBlob( rv ).asInstanceOf[A]
		  
		  unBlob match {
		    case rsrc : mTT.Resource => {
		      getGV( rsrc ) match {
			case Some( cv ) => cv
			case _ => 
			  throw new Exception( "Missing resource conversion" )
		      }
		    }
		  }
		}
		case _ => {
		  //asPatternString( ccl )
		  throw new Exception( "Missing conversion" )
		}
	      }
	    }
	    
	  }
	  
	  def persistenceManifest : Option[PersistenceManifest] = {
	    val sid = Some( ( s : String ) => s )
	    val kvdb = this;
	    Some(
	      new StringXMLDBManifest( dfStoreUnitStr, sid, sid, sid ) {
		override def valueStorageType : String = {
		  kvdb.valueStorageType
		}
		override def continuationStorageType : String = {
		  kvdb.valueStorageType
		}
	      }
	    )
	  }
	}
	
      }

      import Being._
      
      def ptToPt( storeUnitStr : String, a : String, b : String )  = {
	new PersistedStringMGJ( storeUnitStr, a, List( b ) )
      }
      
      def loopBack( storeUnitStr : String ) = {
	ptToPt( storeUnitStr, "localhost", "localhost" )
      }
      
      import scala.collection.immutable.IndexedSeq
      
      type MsgTypes = DTSMSH[String,String,String,A]   
      
      val protoDreqUUID = getUUID()
      val protoDrspUUID = getUUID()    
      
      object MonadicDMsgs extends MsgTypes {
	
	override def protoDreq : DReq = MDGetRequest( aLabel )
	override def protoDrsp : DRsp = MDPutResponse( aLabel )
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


class SimpleStore[A]( 
  val storeUnitStr : String
) extends SimpleStoreScope[A]( ) {
  lazy val ss = loopBack( storeUnitStr )
}

// class SimpleStoreM[A](
//   val storeUnitStr : String,
//   val unitKey : CnxnCtxtLabel[String,String,String]  
// ) extends ForNotationAdapter[SimpleStore,A]
// with BMonad[SimpleStore]
// with MonadPlus[SimpleStore]
// with MonadFilter[SimpleStore]
// {
//   override def unit [S] ( s : S ) : SimpleStore[S] = {
//     val rslt = new SimpleStore[S]( storeUnitStr )
//     reset {
//       rslt.put( unitKey, s )
//     }
//     rslt
//   }
//   override def bind [S,T] (
//     sss : SimpleStore[S],
//     f : S => SimpleStore[T]
//   ) : SimpleStore[T] = {
//     val rslt = new SimpleStore[T]( storeUnitStr )
//   }
// }
