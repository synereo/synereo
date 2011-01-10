// -*- mode: Scala;-*- 
// Filename:    CnxnXML.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 10 03:57:46 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

trait CnxnXML[Namespace,Var,Tag] {
  self : CnxnCtxtInjector[Namespace,Var,Tag] =>

  def toXML( cnxn : CnxnLabel[Namespace,Tag] ) : String = {
    toXML( injectLabel( cnxn ) )
  }
  def toXML( cnxn : CnxnCtxtLabel[Namespace,Var,Tag] ) : String = {
    new XStream( ).toXML( cnxn )
  }
}

