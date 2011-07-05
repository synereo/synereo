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
import scala.collection.Seq
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

trait CoreLoggingScope[M[_], Msg <: java.io.Serializable]
  extends HistoricalContextScope[M,Msg] {
    trait CnxnWriter[A] extends WriterM[A] {
      def tweet( fact : Msg ) : M[Unit]
      def blog( fact : Msg ) : M[Unit]
      def report( fact : Msg ) : M[Unit]    
      def record( fact : Msg ) : M[Unit]
      def log( fact : Msg ) : M[Unit]
      
      def tweetValue[A]( a : A ) : MHCtxt[A]
      def blogValue[A]( a : A ) : MHCtxt[A]
      def reportValue[A]( a : A ) : MHCtxt[A]
      def recordValue[A]( a : A ) : MHCtxt[A]
      def logValue[A]( a : A ) : MHCtxt[A]
      
      def tweet( fact : Msg, severity : Severity.Value ) : M[Unit]
      def blog( fact : Msg, severity : Severity.Value ) : M[Unit]
      def report( fact : Msg, severity : Severity.Value ) : M[Unit]    
      def record( fact : Msg, severity : Severity.Value ) : M[Unit]    
      def log( fact : Msg, severity : Severity.Value ) : M[Unit]        
      
      def tweetValue[A]( a : A, severity : Severity.Value ) : MHCtxt[A]
      def blogValue[A]( a : A, severity : Severity.Value ) : MHCtxt[A]
      def reportValue[A]( a : A, severity : Severity.Value ) : MHCtxt[A]
      def recordValue[A]( a : A, severity : Severity.Value ) : MHCtxt[A]
      def logValue[A]( a : A, severity : Severity.Value ) : MHCtxt[A]
    }  
    
    def beginLogging[A]( a : A ) : CnxnWriter[A]
    def endLogging[A]( cnxnWrtr : CnxnWriter[A] ) : Unit
  }

trait CnxnBasedLoggingScope[
  MD[_],
  MC[+A] <: Seq[A],
  MS[_],
  Msg <: java.io.Serializable
]
extends UUIDOps 
with SeverityOps {
  import SpecialKURIDefaults._
  import CnxnLeafAndBranch._
  import identityConversions._    

  type DisplayM[MD[_]] <: BMonad[MD]
  type CacheM[MC[_]] <: BMonad[MC]
  type StoreM[MS[_]] <: BMonad[MS]  

  trait LoggingDisplayScope
  extends CoreLoggingScope[MD,Msg] {
  }  
  
  trait LoggingCacheScope
  extends CoreLoggingScope[MC,Msg] {
  }  

  trait LoggingStoreScope
  extends CoreLoggingScope[MS,Msg] {
  }    
}


