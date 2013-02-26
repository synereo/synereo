// -*- mode: Scala;-*- 
// Filename:    BaseXConfig.scala 
// Authors:     lgm                                                    
// Creation:    Sat Feb 23 02:21:00 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.xml

import com.biosimilarity.lift.model.store.URIFromConfigurationT
import com.biosimilarity.lift.model.ApplicationDefaults

import java.net.URI

object BaseXConfigInfo
extends XMLStoreConfiguration
with URIFromConfigurationT {  
  override def configFileName: Option[String] = None
  override def configurationDefaults: ConfigurationDefaults =
    ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
  override def getSessionURIFromConfiguration : URI = new URI( URI )
  override def defaultSchemeFromConfiguration : String = {
    _sessionURIFromConfiguration.getScheme
  }
  override def defaultPathFromConfiguration : String = {
    _defaultDB
  }

  lazy val _sessionURIFromConfiguration : URI =
    getSessionURIFromConfiguration
  lazy val _defaultDB = "records"
}
