// -*- mode: Scala;-*- 
// Filename:    DefensiveClassLoader.scala 
// Authors:     lgm                                                    
// Creation:    Sun Aug 17 16:43:17 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import java.util.UUID
import java.net.URI
import java.util.Properties
import java.io.File
import java.io.FileInputStream
import java.io.InputStream
import java.io.OutputStreamWriter
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectStreamClass

case class DefensiveObjectInputStream( in : InputStream ) extends ObjectInputStream( in ) {
  override def resolveClass( desc : ObjectStreamClass ) : Class[_] = {
    try {
      val currentTccl = Thread.currentThread().getContextClassLoader()
      currentTccl.loadClass( desc.getName() )
    } catch {
      case e : Throwable => super.resolveClass(desc);
    }      
  }    
}
