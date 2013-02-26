// -*- mode: Scala;-*- 
// Filename:    AppDefaults.scala 
// Authors:     lgm                                                    
// Creation:    Thu Apr 21 10:25:03 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model

object ApplicationDefaults extends Serializable {
  // BaseX
  implicit val URI : String  =
    "xmldb:basex://localhost:1984/"
  val dbHost : String = "localhost"
  val dbPort : String = "1984"
  val dbUser : String = "admin"
  val dbPwd  : String = "admin"
  implicit val driver : String =
    "org.basex.api.xmldb.BXDatabase"
  implicit val dbRoot : String = "/db"
  implicit val createDB : Boolean = false
  implicit val indent : Boolean = false
  implicit val resourceType : String = "XMLResource"
  val queryServiceType : String = "XPathQueryService"
  val queryServiceVersion : String = "1.0"
  val managementServiceType : String =
    "CollectionManagementService"
  val managementServiceVersion : String = "1.0"  
  val defaultDB : String = "records"
  // Storage conversion mechanism
  val valueStorageType : String = "CnxnCtxtLabel"
  val continuationStorageType : String = "Base64"
  // Logging
  val loggingLevel : String = "Tweet"
  // File system configuration
  implicit val tmpDirStr = "tmp"
}

object BaseXDefaults extends Serializable {
  implicit val URI : String  =
    "xmldb:basex://localhost:1984/"
  val dbHost : String = "localhost"
  val dbPort : String = "1984"
  val dbUser : String = "admin"
  val dbPwd  : String = "admin"
  implicit val driver : String =
    "org.basex.api.xmldb.BXDatabase"
  implicit val dbRoot : String = "/db"
  implicit val createDB : Boolean = false
  implicit val indent : Boolean = false
  implicit val resourceType : String = "XMLResource"
  val queryServiceType : String = "XPathQueryService"
  val queryServiceVersion : String = "1.0"
  val managementServiceType : String =
    "CollectionManagementService"
  val managementServiceVersion : String = "1.0"  
  val valueStorageType : String = "CnxnCtxtLabel"
  val loggingLevel : String = "Tweet"
}

object MongoDefaults extends Serializable {
  // BaseX
  implicit val URI : String  =
    "mongod://localhost:27017/"
  val dbHost : String = "localhost"
  val dbPort : String = "27017"
  val dbUser : String = ""
  val dbPwd  : String = ""
  val dbRoot : String = "~/.mongo/data"
  val createDB : Boolean = false
  val defaultDB : String = "records"
  // Storage conversion mechanism
  val valueStorageType : String = "CnxnCtxtLabel"
  val continuationStorageType : String = "Base64"
  // Logging
  val loggingLevel : String = "Tweet"
  // File system configuration
  implicit val tmpDirStr = "tmp"
}
