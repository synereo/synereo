// -*- mode: Scala;-*- 
// Filename:    AppDefaults.scala 
// Authors:     lgm                                                    
// Creation:    Thu Apr 21 10:25:03 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model

object ApplicationDefaults {
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
  // Storage conversion mechanism
  val valueStorageType : String = "CnxnCtxtLabel"
  // Logging
  val loggingLevel : String = "Tweet"
  // File system configuration
  implicit val tmpDirStr = "tmp"
}

object BaseXDefaults {
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
