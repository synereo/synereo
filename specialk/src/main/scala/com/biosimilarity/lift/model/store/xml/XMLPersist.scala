// -*- mode: Scala;-*- 
// Filename:    XMLPersist.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 10 02:03:57 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.xml

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.CnxnLabel
import com.biosimilarity.lift.model.store.OntologicalStatus
import com.biosimilarity.lift.model.store.Factual
import com.biosimilarity.lift.model.store.Hypothetical
import com.biosimilarity.lift.model.store.Theoretical
import com.biosimilarity.lift.model.store.CnxnLeaf
//import com.biosimilarity.lift.model.store.CCnxnLeaf
import com.biosimilarity.lift.model.store.AbstractCnxnBranch
import com.biosimilarity.lift.model.store.CnxnBranch
//import com.biosimilarity.lift.model.store.CCnxnBranch
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.biosimilarity.lift.model.store.CnxnCtxtLeaf
//import com.biosimilarity.lift.model.store.CCnxnCtxtLeaf
import com.biosimilarity.lift.model.store.AbstractCnxnCtxtBranch
import com.biosimilarity.lift.model.store.CnxnCtxtBranch
//import com.biosimilarity.lift.model.store.CCnxnCtxtBranch
import com.biosimilarity.lift.model.store.CnxnCtxtInjector
import com.biosimilarity.lift.model.store.Cnxn
import com.biosimilarity.lift.model.store.CCnxn
import com.biosimilarity.lift.model.store.CnxnXML
import com.biosimilarity.lift.model.store.Blobify
import com.biosimilarity.lift.model.store.CnxnXQuery
import com.biosimilarity.lift.lib._

import org.basex.api.xmldb.BXCollection

//import org.exist.storage.DBBroker

import org.xmldb.api.base._
import org.xmldb.api.modules._
import org.xmldb.api._

//import net.lag.configgy._
import com.typesafe.config._

import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.xml._

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import javax.xml.transform.OutputKeys
import java.util.UUID
import java.io.File

trait XMLStoreConfiguration
extends ConfigurationTrampoline {
  def URI                      : String =
    configurationFromFile.get( "URI" ).getOrElse( bail() )
  def dbHost                   : String =
    configurationFromFile.get( "dbHost" ).getOrElse( bail() )
  def dbPort                   : String =
    configurationFromFile.get( "dbPort" ).getOrElse( bail() )
  def dbUser                   : String =
    configurationFromFile.get( "dbUser" ).getOrElse( bail() )
  def dbPwd                    : String =
    configurationFromFile.get( "dbPwd" ).getOrElse( bail() )
  def driver                   : String =
    configurationFromFile.get( "driver" ).getOrElse( bail() )
  def dbRoot                   : String =
    configurationFromFile.get( "dbRoot" ).getOrElse( bail() )
  def createDB                 : Boolean =
    configurationFromFile.get( "createDB" ).getOrElse( bail() ).toBoolean
  def indent                   : Boolean =
    configurationFromFile.get( "indent" ).getOrElse( bail() ).toBoolean
  def resourceType             : String =
    configurationFromFile.get( "resourceType" ).getOrElse( bail() )
  def queryServiceType         : String =
    configurationFromFile.get( "queryServiceType" ).getOrElse( bail() )
  def queryServiceVersion      : String =
    configurationFromFile.get( "queryServiceVersion" ).getOrElse( bail() )
  def managementServiceType    : String =
    configurationFromFile.get( "managementServiceType" ).getOrElse( bail() )
  def managementServiceVersion : String =
    configurationFromFile.get( "managementServiceVersion" ).getOrElse( bail() )  

  def valueStorageType : String =
    configurationFromFile.get( "valueStorageType" ).getOrElse( bail() )  
  def continuationStorageType : String =
    configurationFromFile.get( "continuationStorageType" ).getOrElse( bail() )  
  def tmpDirStr : String =
    configurationFromFile.get( "tmpDirStr" ).getOrElse( bail() )
}

trait XMLStoreConfigurationProxy
extends XMLStoreConfiguration {
  def underlyingConfiguration : XMLStoreConfiguration
  
  override def URI                      : String = underlyingConfiguration.URI
  override def dbHost                   : String = underlyingConfiguration.dbHost
  override def dbPort                   : String = underlyingConfiguration.dbPort
  override def dbUser                   : String = underlyingConfiguration.dbUser
  override def dbPwd                    : String = underlyingConfiguration.dbPwd
  override def driver                   : String = underlyingConfiguration.driver
  override def dbRoot                   : String = underlyingConfiguration.dbRoot
  override def createDB                 : Boolean = underlyingConfiguration.createDB
  override def indent                   : Boolean = underlyingConfiguration.indent
  override def resourceType             : String = underlyingConfiguration.resourceType
  override def queryServiceType         : String = underlyingConfiguration.queryServiceType
  override def queryServiceVersion      : String = underlyingConfiguration.queryServiceVersion
  override def managementServiceType    : String = underlyingConfiguration.managementServiceType
  override def managementServiceVersion : String = underlyingConfiguration.managementServiceVersion

  override def valueStorageType         : String = underlyingConfiguration.valueStorageType  
  override def continuationStorageType  : String = underlyingConfiguration.continuationStorageType  
  override def tmpDirStr                : String = underlyingConfiguration.tmpDirStr

  override def configFileName  : Option[String] =
    underlyingConfiguration.configFileName
  override def configurationDefaults: ConfigurationDefaults =
    underlyingConfiguration.configurationDefaults
}

trait StdXMLStoreConfiguration extends XMLStoreConfigurationProxy {
  final override val underlyingConfiguration = BaseXConfigInfo
}

trait XMLStoreDefaults

trait XMLStore
extends XMLStoreConfiguration {  
  self : UUIDOps =>

    //override type ConfigurationDefaults = XMLStoreDefaults

    //override def configFileName : Option[String] = Some(
    //"xmlStore.conf" )
    override def configFileName : Option[String] = None

  def driverClass : Class[_] = {
    Class.forName( driver )
  }

  def database : Database = {
    val db = driverClass.newInstance().asInstanceOf[Database]
    DatabaseManager.registerDatabase( db )
    if ( createDB ) {
      db.setProperty( "create-database", createDB.toString )
    }
    db
  }

  def getCollection( createIfMissing : Boolean )( xmlCollStr : String ) : Option[Collection] = {
    val xmlColl : Option[Collection] =          
      {
	val coll = DatabaseManager.getCollection( URI + xmlCollStr ) 
	if ( coll == null ) {
	  if ( createIfMissing ) {
	    val root : Collection = DatabaseManager.getCollection( URI + dbRoot )
	    val mgtService : CollectionManagementService =
	      root.getService(
		managementServiceType,
		managementServiceVersion
	      ).asInstanceOf[CollectionManagementService]

	    Some(
	      mgtService.createCollection(
		xmlCollStr.substring( "/db".length() )
	      )
	    )
	  }
	  else {
	    None
	  }
	}
	else {
	  Some( coll )
	}
      }

    for( xColl <- xmlColl ) {
      if ( indent ) {
	xColl.setProperty(OutputKeys.INDENT, "yes")
      } 
      else {
	xColl.setProperty(OutputKeys.INDENT, "no")
      }
    }

    xmlColl
  }

  def getResource( xmlColl : Collection )( xmlRsrcStr : String )
  : Option[XMLResource] = {
    val xmlRsrc =
      xmlColl.getResource( xmlRsrcStr ).asInstanceOf[XMLResource]
    if ( xmlRsrc == null ) {
      None
    }
    else {
      Some( xmlRsrc )
    }
  }

  def createResource( xmlColl : Collection, xmlRsrcStr : String )
  : Option[XMLResource] = {
    val document : XMLResource =
      xmlColl.createResource( null, resourceType ).asInstanceOf[XMLResource]

    val f : File = new File( xmlRsrcStr )

    if ( f.canRead() ) {
      document.setContent( f )
      xmlColl.storeResource( document )
      Some( document )
    }
    else {      
      println( "cannot read file " + xmlRsrcStr )
      None
    }    
  }

  def createResource( xmlColl : Collection, xmlRsrcFile : java.io.File )
  : Option[XMLResource] = {

    val document : XMLResource =
      xmlColl.createResource( null, resourceType ).asInstanceOf[XMLResource]

    if ( xmlRsrcFile.canRead() ) {
      document.setContent( xmlRsrcFile )
      xmlColl.storeResource( document )
      Some( document )
    }
    else {      
      println( "cannot read file " + xmlRsrcFile.getName )
      None
    }    
  }

  def createResourceFromContent(
    xmlColl : Collection,
    xmlRsrcContentStr : String
  ) : Option[XMLResource] = {

    val document : XMLResource =
      xmlColl.createResource( null, resourceType ).asInstanceOf[XMLResource]

    document.setContent( xmlRsrcContentStr )
    xmlColl.storeResource( document )
    Some( document )
  }

  def createResource( xmlColl : Collection, xmlRsrcContent : Elem )
  : Option[XMLResource] = {

    createResourceFromContent( xmlColl, xmlRsrcContent.toString )
  }  

  def getQueryService( xmlColl : Collection )(
    qrySrvcType : String, qrySrvcVersion : String
  ) : Service = {
    val srvc = xmlColl.getService( qrySrvcType, qrySrvcVersion )
    // if ( indent ) {
//       srvc.setProperty( "indent", "yes" )
//     }
//     else {
//       srvc.setProperty( "indent", "no" )
//     }
    srvc
  }

  def getQueryServiceC( xmlColl : Collection ) : Service = {
    getQueryService( xmlColl )( queryServiceType, queryServiceVersion )
  }

  def execute( xmlColl : Collection )(
    srvc : Service
  )(
    qry : String
  ) : ResourceSet = {
    srvc match {
      // case xqSrvc : XQueryService => {
// 	xqSrvc.execute( xqSrvc.compile( qry ) )
//       }
      case xqSrvc : XPathQueryService => {
	xqSrvc.query( qry )
      }
      case _ => {
	throw new Exception( "execute not supported" )
      }
    }
  }

}

trait XMLIfy[Namespace,Var] {
  object theXMLIfier
    extends CnxnXML [Namespace,Var,String]
    with CnxnCtxtInjector[Namespace,Var,String]
    with Blobify
    with UUIDOps
    with Serializable

  def xmlIfier : CnxnXML [Namespace,Var,String] = {
    theXMLIfier
  }  
}

trait CnxnStorage[Namespace,Var,Tag]
extends XMLIfy[Namespace,Var] {
  self : XMLStore
    with UUIDOps =>
    
    def tmpDirStr : String
  
  def store( xmlCollStr : String )( cnxn : CnxnCtxtLabel[Namespace,Var,String] ) : Unit = {
    import java.io.FileWriter
    import java.io.BufferedWriter

    for( xmlColl <- getCollection( true )( xmlCollStr ) ) {
      val xmlRsrcId = getUUID()
      val xmlRsrcStr = tmpDirStr + "/" + xmlRsrcId.toString + ".xml"
      val xmlRsrcFile = new File( xmlRsrcStr )
      val fstream : FileWriter = new FileWriter( xmlRsrcFile )
      val out : BufferedWriter = new BufferedWriter( fstream )
      //out.write( toXML( cnxn ) ) -- Serialization-based storage
      out.write( xmlIfier.asXML( cnxn ).toString ) // Data-binding-based storage
      out.close
    
      val xrsrc = createResource( xmlColl, xmlRsrcFile )
    }
  }
  def update( xmlCollStr : String )(
    cnxn : CnxnCtxtLabel[Namespace,Var,String]
  ) : Unit = {
    store( xmlCollStr )( cnxn )
  }
}

object CnxnStorageDefaults {
  implicit val tmpDirStr = "tmp"
}


